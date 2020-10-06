#' Calculate swept area biomass
#'
#' Description
#'
#' @param data Data frame. Survey data pull from svdbs with applied conversion factors
#' @param areaPolygon sf object. Defining the areas by which to estimate biomass.
#' @param areaDescription Character string. The name of the column in areaPolygon that describes the area (eg. "EPU","STRATA")
#' @param filterByArea Character Vector. Vecor of area names. Names found in the \code{areaPolygon} in \code{areaDescription} Column
#' @param filterBySeason Character Vector. Vecor of season names. Names found in the SEASON column of \code{data}
#' @param species Character Vector. Vector of species SVSPP codes.
#' @param merge.sex Logical value to merge sexed species such as dogfish.'
#' @param poststrat Logical value indicating whether the original strata design was
#'used or not.  Changes the calculation for variance.
#' @param q Table of survey catchability with a column of group names and a column of
#'catchabilities.  If not provided, assumes a q of 1 for each group (Minimum swept area
#'estimates).
#' @param a The average swept area of the trawl.  Default value is the swept area of a
#'standard NOAA Ship Albatross IV tow.
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


swept_area_biomass <- function(data,areaPolygon,areaDescription,filterByArea,filterBySeason,species="all",merge.sex=T,poststrat=F,q=NULL,a=0.0384) {


  strat.area <- survdat::get_area(areaPolygon,areaDescription)
  # post stratify
  message("Post stratifying ...")
  survdata <- survdat::post_strat(data, areaPolygon, strata.col=areaDescription)

  #if length((filterByArea)
  filteredData <- survdata[SEASON == filterBySeason & get(areaDescription) %in% filterByArea, ]

  #Run stratification prep
  message("Prepping  ...")
  survdatPrep <- survdat::strat_prep(filteredData, strat.area, strat.col = areaDescription)


  stratGroupMean <- survdat::strat_mean(survdatPrep, groups = species, group.col = 'SVSPP',strat.col = 'REGION',
                                   poststrat = poststrat, merge.sex = merge.sex)

  #Calculate total biomass/abundance estimates
  total.biomass <- survdat::swept_area(survdat=survdatPrep, stratmean=stratGroupMean,
                                       q = q, strat.col = 'REGION')


  return(total.biomass)
}

