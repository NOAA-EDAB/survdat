#'Prepare survey data for calculating stratified mean
#'
#'Calculates the number of tows per strata and strata weight.  Run before
#'sub setting the data by species of interest to ensure all stations are accounted.
#'
#'
#' @param areaPolygon sf object or character string. Default = "NEFSC strata". The default option uses the survey strata shapefile bundled with the package.
#' To use any other shapefile for stratification, the shapefile must be read in as an \link[sf]{sf} object and the \code{areaDescription} argument must be specified.
#' @param areaDescription Character String. Column name from \code{areaPolygon}
#'                       that contains the strata designations.
#' @param surveyData Data table. NEFSC survey data generated by \code{get_survdat_data.R}
#' @param filterByArea Numeric vector. Set of areas to subset from the
#'  \code{areaDescription} of the \code{areaPolygon}.
#' @param filterBySeason Character string. Which seasons of the \code{surveyData}
#'  should be included.  Choices include "SPRING", "FALL", or "all".
#'
#' @return Returns a prepSurvey data object with added columns for the number of
#'  tows (ntows) and stratum weights (W.h).
#'
#' @importFrom data.table "key"
#'
#'@family survdat
#'
#'\@examples
#' \dontrun{
#' # Called internally
#' }
#'
#' @noRd


strat_prep <- function (surveyData, areaPolygon = "NEFSC strata",
                        areaDescription = "STRATA", filterByArea = "all",
                        filterBySeason = "all") {

  # Break link to original data set so no changes are made to original
  surveyData <- data.table::copy(surveyData)

  # Calculate the proportional areas
  # Use original stratified design and built-in shapefile or flag for post
  # stratification
  if(!is(areaPolygon, 'sf')){
    if(areaPolygon == 'NEFSC strata'){
      areaPolygon <- sf::st_read(dsn = system.file("extdata", "strata.shp",
                                                   package = "survdat"), quiet = T)
      poststratFlag <- F
    }
  } else poststratFlag <- T

  # Calculate area of the polygons
  polygonArea <- survdat::get_area(areaPolygon, areaDescription)

  # post stratify if necessary
  if(poststratFlag){
    message("Post stratifying ...")
    surveyData <- survdat:::post_strat(surveyData, areaPolygon, areaDescription)
  } else {
    #Add extra column to original data to mimic what happens when post-stratifying
    surveyData <- surveyData[, areaDescription := STRATUM]
    data.table::setnames(surveyData, 'areaDescription', areaDescription)
  }

  #FilterData
  if(filterByArea[1] != "all" | filterBySeason[1] != "all"){
    message("Filtering data ...")
    seasonFlag <- T
  }

  if(filterBySeason[1] == 'all'){
    filterBySeason <- unique(surveyData[, SEASON])
    seasonFlag <- F
  }

  # check to create all areas
  if (length(filterByArea)==1) {
    if (filterByArea == "all") {
      filterByArea <- areaPolygon %>%
        sf::st_drop_geometry() %>%
        dplyr::select(areaDescription) %>%
        unlist() %>%
        unique()
      if (is.factor(filterByArea)){ # convert to levels
        filterByArea <- levels(filterByArea)
      }
    }
  }
  print(filterByArea)

  filteredData <- surveyData[SEASON %in% filterBySeason & get(areaDescription) %in% filterByArea, ]

  #Change to generic names for calculations
  data.table::setnames(filteredData, areaDescription, 'STRAT')
  data.table::setnames(polygonArea, c("STRATUM", "Area"), c('STRAT', 'S.AREA'))

  # if not using original stratification need to preserve unique key
  if(!is.null(areaPolygon)){
    filteredData[, STATION2 := as.numeric(paste0(STRATUM, STATION))]
    data.table::setnames(filteredData, c('STATION', 'STATION2'),
                         c('ORIGSTATION', 'STATION'))
  }

  #Station data - Finds list of distinct stations sampled through time
  data.table::setkey(filteredData, CRUISE6, STRAT, STATION)
  stations <- unique(filteredData, by = key(filteredData))
  stations <- stations[, list(YEAR, CRUISE6, STRAT, STATION)]

  #x %>% dplyr::distinct(YEAR,CRUISE6,STRAT,STATION)
  # Count the number of stations in each year for each Region
  if(seasonFlag){
    data.table::setkey(stations, YEAR, CRUISE6, STRAT)
  } else { data.table::setkey(stations, YEAR, STRAT)}
  stations[, ntows := length(STATION), by = key(stations)]

  #Merge stations and area
  stations <- base::merge(stations, polygonArea, by = 'STRAT', all.x = T)

  #Calculate stratum weight
  if(seasonFlag){
    keyoff <- c('YEAR', 'CRUISE6')
  } else {
    keyoff <- 'YEAR'
  }
  data.table::setkeyv(stations, c(keyoff, 'STRAT'))
  strat.year <- unique(stations, by = key(stations))
  strat.year[, c('STATION', 'ntows') := NULL]
  strat.year[, W.h := S.AREA / sum(S.AREA, na.rm = T), by = keyoff]
  strat.year[, W.h := as.vector(W.h)] #Drops the units from the area
  strat.year[is.na(W.h), W.h := 0]
  strat.year[, S.AREA := NULL]
  if(!seasonFlag) strat.year[, CRUISE6 := NULL]

  #Merge back
  stations <- merge(stations, strat.year, by = key(stations))

  #Merge catch with station data
  prepData <- merge(filteredData, stations, by = c('YEAR', 'CRUISE6', 'STRAT', 'STATION'))

  data.table::setnames(prepData, c('STRAT', 'S.AREA'),
           c(areaDescription, "Area"))

  # Restore original station number if not using the original stratified design
  if(!is.null(areaPolygon)){
    data.table::setnames(prepData, c('STATION', 'ORIGSTATION'), c('STATION2', 'STATION'))
    prepData[, STATION2 := NULL]
  }

  return(prepData[])
}
