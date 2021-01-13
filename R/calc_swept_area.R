#' Calculate swept area biomass
#'
#' Calculates the biomass. Details found here ...
#'
#' @inheritParams calc_stratified_mean
#' @param q Data frame. Table of survey catchabilities with a column corresponding
#'  to \code{groupDescription} and a column of catchabilities.  If NULL, assumes
#'  a \code{q} of 1 for each \code{groupDescription} (Minimum swept area estimates).
#' @param a Numeric. The average swept area of the trawl.  Default value is the
#'  swept area of a standard NOAA Ship Albatross IV tow.
#'
#' @return
#'
#'@family survdat
#'
#' @examples
#' \dontrun{
#' # Pull data and apply conversion corrections
#' data <- get_survdat_data(channel)
#' # Calculate swept area biomass for specific survey strata for the SPRING season
#' calc_swept_area(data=data, filterByArea=c(1220, 1240, 1260:1290,1360:1400),filterBySeason = "SPRING")
#'
#' # Calculate stratified mean for area defined by EPU regions, for all seasons ("SPRING", "FALL") and return in Tidy format
#' # Read in EPU shapefile (loaded as part of the package)
#' area <- sf::st_read(dsn = system.file("extdata","EPU.shp",package="survdat"),quiet=T)
#' calc_swept_area(data=data, areaPolygon=area, areaDescription="EPU", filterByArea="all",filterBySeason = "all",tidy=T)
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

    #create tidy data set
    if(tidy){
        message("Tidying data  ...")
        tidyData <- data.table::melt.data.table(sweptareaData, id.vars = c('YEAR',
                                                                           'SVSPP'),
                                                measure.vars = c('strat.biomass',
                                                                 'biomass.var',
                                                                 'strat.abund',
                                                                 'abund.var',
                                                                 'tot.biomass',
                                                                 'tot.bio.var',
                                                                 'tot.abundance',
                                                                 'tot.abund.var'))
        tidyData[variable == 'strat.biomass', units := 'kg tow^-1']
        tidyData[variable == 'biomass.var',   units := '(kg tow^-1)^2']
        tidyData[variable == 'strat.abund',   units := 'number']
        tidyData[variable == 'abund.var',     units := 'numbers^2']
        tidyData[variable == 'tot.biomass',   units := 'kg']
        tidyData[variable == 'tot.bio.var',   units := 'kg^2']
        tidyData[variable == 'tot.abundance', units := 'number']
        tidyData[variable == 'tot.abund.var', units := 'numbers^2']

        sweptareaData <- tidyData
    }

    sweptareaData[]

    return(sweptareaData)
}






