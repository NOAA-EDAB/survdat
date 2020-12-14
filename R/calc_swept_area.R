#' Calculate swept area biomass
#'
#' Calculates the biomass
#'
#' @param surveyData Data frame. Survey data pull from svdbs with applied conversion factors
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

calc_swept_area <- function(surveyData, areaPolygon = 'NEFSC strata', 
                            areaDescription = 'STRATA', filterByArea = "all", 
                            filterBySeason, groupDescription = "SVSPP", 
                            filterByGroup = "all", mergesexFlag = T, 
                            tidy = F, q = NULL, a = 0.0384) {
    
    #Run stratified mean
    stratmeanData <- calc_stratified_mean(surveyData, areaPolygon, areaDescription, 
                                          filterByArea, filterBySeason, 
                                          groupDescription, filterByGroup, 
                                          mergesexFlag, returnPrepData = T)
    
    #Calculate total biomass/abundance estimates
    message("Calculating Swept Area Estimate  ...")
    sweptareaData <- survdat::swept_area(prepData = stratmeanData$prepData, 
                                         stratmeanData = stratmeanData$stratmeanData,
                                         q = q, areaDescription = areaDescription, 
                                         groupDescription = groupDescription)
    
    return(sweptareaData)
}
    
    
    
    
    
    
    