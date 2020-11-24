#' Calculate swept area biomass
#'
#' Calculates the biomass
#'
#' @param data Data frame. Survey data pull from svdbs with applied conversion factors
#' @param areaPolygon sf object. Defining the areas by which to estimate biomass.
#' @param areaDescription Character string. The name of the column in areaPolygon that describes the area (eg. "EPU","STRATA")
#' @param filterByArea Character Vector. Vecor of area names. Names found in the \code{areaPolygon} in \code{areaDescription} Column. Biomass estimates will be aggregated over all areas specified in \code{filterByArea} (Default = "all")
#' @param filterBySeason Character Vector. Vector of season names. Names found in the SEASON column of \code{data}
#' @param species Character Vector. Vector of species SVSPP codes.
#' @param merge.sex Boolean. Merge sexed species such as dogfish. (Default = T)
#' @param poststrat Boolean. Indicating whether the original strata design was
#'used or not.  Changes the calculation for variance. (Default = F)
#' @param q Table of survey catchability with a column of group names and a column of
#'catchabilities.  If not provided, assumes a q of 1 for each group (Minimum swept area
#'estimates).
#' @param a The average swept area of the trawl.  Default value is the swept area of a
#'standard NOAA Ship Albatross IV tow.
#' @param tidy Boolean. Return output in long format (Default = F)
#'
#' @return
#'
#'
#' @examples
#' \dontrun{
#' # pull data and apply conversion corrections
#' data <- survdatData
#'
#' # read in "strata" shapefile
#' area <- sf::st_read(dsn = system.file("extdata","strata.shp",package="survdat"),quiet=T)
#'
#' # estimate swept area biomass in the SPRING in STRATA = 1220, 1240, 1260:1290,1360:1400 for species = 300:310
#' swa <-swept_area_biomass(data=data, areaPolygon = area, areaDescription="STRATA", filterByArea=c(1220, 1240, 1260:1290,1360:1400),filterBySeason = "SPRING", species=c(300:310))
#'
#' }
#'
#'
#' @export


swept_area_biomass <- function(data, areaPolygon = NULL, areaDescription, 
                               filterByArea = "all", filterBySeason, species = "all", 
                               crs = "+proj=lcc +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-72 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0", 
                               merge.sex = T, q = NULL, a =0.0384, 
                               tidy = F) {

  #Use original stratified design and built-in shapefile
  if(is.null(areaPolygon)){
    areaPolygon <- sf::st_read(dsn = system.file("extdata", "strata.shp", 
                                                 package = "survdat"), quiet = T)
    data$STRATUM <- as.numeric(data$STRATUM)
    poststrat <- F
  } else {poststrat <- T} #Not using original stratification
  
  strat.area <- survdat::get_area(areaPolygon, areaDescription, crs = crs)
  
  # post stratify
  if(poststrat){
    message("Post stratifying ...")
    survdata <- survdat::post_strat(data, areaPolygon, strata.col = areaDescription,
                                    crs = crs)
  } else {
    survdata <- data[, areaDescription := STRATUM]
    data.table::setnames(survdata, 'areaDescription', areaDescription)
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

  filteredData <- survdata[SEASON == filterBySeason & get(areaDescription) %in% filterByArea, ]

  #Run stratification prep
  message("Prepping  ...")
  survdatPrep <- survdat::strat_prep(filteredData, strat.area, strat.col = areaDescription)


  stratGroupMean <- survdat::strat_mean(survdatPrep, groups = species, 
                                        group.col = 'SVSPP', strat.col = areaDescription,
                                        poststrat = poststrat, merge.sex = merge.sex)

  #Calculate total biomass/abundance estimates
  total.biomass <- survdat::swept_area(survdat=survdatPrep, stratmean=stratGroupMean,
                                       q = q, strat.col = areaDescription)

  # create tidy data

  return(total.biomass)
}

