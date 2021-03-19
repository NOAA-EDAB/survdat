#' Calculate stratified mean
#'
#' Calculates the stratified mean. Details of method found here ...
#'
#' @inheritParams strat_prep
#' @inheritParams strat_mean
#' @param tidy Boolean. Return output in long format (Default = F).
#' @param returnPrepData Boolean. Return both \code{stratmeanData} and \code{prepData}
#'   as a list object. The default (F) returns only the \code{stratmeanData} as a
#'   \code{data.table}.
#'
#' @return
#'
#'@family survdat
#'
#' @examples
#' \dontrun{
#' # Pull data and apply conversion corrections
#' data <- get_survdat_data(channel)
#' # Calculate stratified mean for specific survey strata for the SPRING season
#' calc_stratified_mean(surveyData=data$survdat, filterByArea=c(1220, 1240, 1260:1290,1360:1400),filterBySeason = "SPRING")
#'
#' # Calculate stratified mean for area defined by EPU regions, for all seasons ("SPRING", "FALL")
#' # Read in EPU shapefile (loaded as part of the package)
#' area <- sf::st_read(dsn = system.file("extdata","EPU.shp",package="survdat"),quiet=T)
#' calc_stratified_mean(surveyData=data$survdat, areaPolygon=area, areaDescription="EPU", filterByArea="all",filterBySeason = "all")
#'
#' }
#'
#'
#' @export


calc_stratified_mean <- function(surveyData, areaPolygon = 'NEFSC strata',
                                 areaDescription = 'STRATA', filterByArea = "all",
                                 filterBySeason, groupDescription = "SVSPP",
                                 filterByGroup = "all", mergesexFlag = T,
                                 tidy = F, returnPrepData = F) {

  # Use original stratified design and built-in shapefile
  if(!is(areaPolygon, 'sf')) {
    if (is.character(areaPolygon)) { # not sf but a string
      if(areaPolygon == 'NEFSC strata') {
        poststratFlag <- F
      } else {
        message("areaPolygon currently only takes \"NEFSC strata\" as character string option")
        stop("areaPolygon incorrectly defined")
      }
    } else { # not sf and not a string
      message("areaPolygon must be an sf object. For example")
      message("areaPolygon <- sf::st_read(dsn = \"shapefile.shp\"")
      stop("areaPolygon incorrectly defined")

    }
  } else {
    poststratFlag <- T
  }

  #Run stratification prep
  message("Prepping data ...")
  prepData <- survdat::strat_prep(surveyData, areaPolygon, areaDescription,
                                  filterByArea, filterBySeason)

  #Calculate stratified mean
  message("Calculating Stratified Mean  ...")
  #Check if calculating mean base on all station or by season
  if(filterBySeason[1] == "all"){seasonFlag <- F}else{seasonFlag <- T}

  stratmeanData <- survdat::strat_mean(prepData, groupDescription, filterByGroup,
                                       mergesexFlag, seasonFlag = seasonFlag,
                                       areaDescription, poststratFlag)

  # create tidy data
  if(tidy){
    message("Tidying data  ...")
    tidyData <- data.table::melt.data.table(stratmeanData, id.vars = c('YEAR',
                                                                       groupDescription,
                                                                       'SEASON'),
                                            measure.vars = c('strat.biomass',
                                                             'biomass.var',
                                                             'biomass.SE',
                                                             'strat.abund',
                                                             'abund.var',
                                                             'abund.SE'))
    tidyData[variable == 'strat.biomass', units := 'kg tow^-1']
    tidyData[variable == 'biomass.var',   units := '(kg tow^-1)^2']
    tidyData[variable == 'biomass.SE',    units := 'kg tow^-1']
    tidyData[variable == 'strat.abund',   units := 'number tow^-1']
    tidyData[variable == 'abund.var',     units := '(numbers tow^-1)^2']
    tidyData[variable == 'abund.SE',      units := 'kg tow^-1']
    stratmeanData <- tidyData
  }

  if(returnPrepData) stratmeanData <- list(stratmeanData = stratmeanData,
                                           prepData = prepData)

  return(stratmeanData[])
}

