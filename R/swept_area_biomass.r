#' Calculate swept area biomass
#'
#' Description
#'
#' @param data Data frame. Survey data pull from svdbs with applied conversion factors
#' @param areaPolygon sf object. Defining the areas by which to estimate biomass.
#' @param areaDescription Character string. The name of the column in areaPolygon that describes the area (eg. "EPU","STRATA")
#' @param filterByArea Character Vector. Vecor of area names. Names found in the \code{areaPolygon} in \code{areaDescription} Column
#' #' @param filterBySeason Character Vector. Vecor of season names. Names found in the SEASON column of \code{data}
#' @param species Character Vector. Vector of species SVSPP codes.
#'
#'
#' @return
#'
#'
#' @examples
#' \dontrun{
#'
#'
#'
#' }
#'
#'
#' @export


swept_area_biomass <- function(data,areaPolygon,areaDescription,filterByArea,filterBySeason,species) {


  strat.area <- survdat::get_area(areaPolygon,areaDescription)
  # post stratify
  message("Post stratifying ...")
  survdata <- survdat::post_strat(data, areaPolygon, strata.col=areaDescription)

  filteredData <- survdata[SEASON == filterBySeason & get(areaDescription) == filterByArea, ]

  #Run stratification prep
  message("Prepping  ...")
  prep <- survdat::strat_prep(filteredData, strat.area, strat.col = areaDescription)


  groupMean <- survdat::strat_mean(prep, groups = species, group.col = 'SVSPP',
                                   strat.col = 'REGION')

  #Calculate total biomass/abundance estimates
  total.biomass <- survdat::swept_area(prep, groupMean, strat.col = 'REGION')


  return(total.biomass)
}

