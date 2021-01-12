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


calc_stratified_mean <- function(surveyData, areaPolygon = 'NEFSC strata',
                                 areaDescription = 'STRATA', filterByArea = "all",
                                 filterBySeason, groupDescription = "SVSPP",
                                 filterByGroup = "all", mergesexFlag = T,
                                 tidy = F, returnPrepData = F) {

  # Use original stratified design and built-in shapefile
  if(!is(areaPolygon, 'sf')){
    if(areaPolygon == 'NEFSC strata') poststratFlag <- F
  } else poststratFlag <- T

  #Run stratification prep
  message("Prepping data ...")
  prepData <- survdat::strat_prep(surveyData, areaPolygon, areaDescription,
                                  filterByArea, filterBySeason)

  #Calculate stratified mean
  message("Calculating Stratified Mean  ...")
  #Pick up here...
  stratmeanData <- survdat::strat_mean(prepData, groupDescription, filterByGroup,
                                       mergesexFlag, areaDescription, poststratFlag)

  # create tidy data
  if(tidy){
    message("Tidying data  ...")
    tidyData <- data.table::melt.data.table(stratmeanData, id.vars = c('YEAR',
                                                                       'SVSPP'),
                                            measure.vars = c('strat.biomass',
                                                             'biomass.var',
                                                             'strat.abund',
                                                             'abund.var'))
    tidyData[variable == 'strat.biomass', units := 'kg tow^-1']
    tidyData[variable == 'biomass.var',   units := '(kg tow^-1)^2']
    tidyData[variable == 'strat.abund',   units := 'number tow^-1']
    tidyData[variable == 'abund.var',     units := '(numbers tow^-1)^2']
    stratmeanData <- tidyData
  }

  stratmeanData[]

  if(returnPrepData) stratmeanData <- list(stratmeanData = stratmeanData,
                                           prepData = prepData)

  return(stratmeanData)
}

