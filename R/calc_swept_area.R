#' Calculate swept area biomass
#'
#' This function is a wrapper of intermediate functions \code{\link{calc_stratified_mean}} and \code{\link{swept_area}}.
#' The \code{\link{calc_stratified_mean}} function is used to calculate the stratified mean biomass and abundance for each group (e.g. species) and season, as well as the variance and standard error of the mean.
#' The resulting output of \code{\link{calc_stratified_mean}} is then passed to the \code{\link{swept_area}} function which calculates the total biomass/abundance estimates.
#'
#'
#' @inheritParams calc_stratified_mean
#' @param q Data frame. Table of survey catchabilities with a column corresponding
#'  to \code{groupDescription} and a column of catchabilities.  If NULL, assumes
#'  a \code{q} of 1 for each \code{groupDescription} (Minimum swept area estimates).
#' @param a Numeric. The average swept area of the trawl.  Default value is the
#'  swept area of a standard NOAA Ship Albatross IV tow.(\href{https://repository.library.noaa.gov/view/noaa/25243}{NEFSC, 2006})
#'
#' @section Source:
#'
#' 43rd Northeast Regional Stock Assessment Workshop (43rd SAW). 2006.  \href{https://repository.library.noaa.gov/view/noaa/25243}{**43rd SAW
#' assessment report.** US Dep. Commer., Northeast Fish. Sci. Cent. Ref. Doc. 06-25; 400 p.}
#'
#' @return data frame
#'
#' @family survdat
#'
#' @importFrom data.table :=
#' @import dplyr
#' @import tidyr
#'
#' @examples
#' \dontrun{
#' # Pull data and apply conversion corrections
#' data <- get_survdat_data(channel)
#' # Calculate swept area biomass for specific survey strata for the SPRING season
#' calc_swept_area(surveyData=data$survdat, filterByArea=c(1220, 1240, 1260:1290,1360:1400),filterBySeason = "SPRING")
#'
#' # Calculate stratified mean for area defined by EPU regions, for all seasons ("SPRING", "FALL") and return in Tidy format
#' # Read in EPU shapefile (loaded as part of the package)
#' area <- sf::st_read(dsn = system.file("extdata","EPU.shp",package="survdat"),quiet=T)
#' calc_swept_area(surveyData=data$survdat, areaPolygon=area, areaDescription="EPU", filterByArea="all",filterBySeason = "all",tidy=T)
#'
#' }
#'
#'
#' @export

calc_swept_area <- function(
  surveyData,
  areaPolygon = 'NEFSC strata',
  areaDescription = 'STRATA',
  filterByArea = "all",
  filterBySeason = "all", # ISSUE 79 FIX: Default to "all" to prevent missing argument errors
  groupDescription = "SVSPP",
  filterByGroup = "all",
  mergesexFlag = T,
  tidy = F,
  q = NULL,
  a = 0.0384
) {
  # -----------------------------------------------------------------------
  # ISSUE 79 FIX: Check for required fields early to prevent cryptic crashes
  # -----------------------------------------------------------------------
  required_cols <- c(
    "YEAR",
    "SEASON",
    "STRATUM",
    "TOW",
    "ABUNDANCE",
    "BIOMASS",
    groupDescription
  )
  missing_cols <- setdiff(required_cols, names(surveyData))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "Cannot calculate swept area. The surveyData object is missing required fields: %s",
      paste(missing_cols, collapse = ", ")
    ))
  }
  # -----------------------------------------------------------------------
  # ISSUE 79 FIX: Ensure input is a data.table to prevent legacy := crashes
  # -----------------------------------------------------------------------
  if (!data.table::is.data.table(surveyData)) {
    surveyData <- data.table::as.data.table(surveyData)
  }
  # -----------------------------------------------------------------------
  # Run Stratified Mean
  # -----------------------------------------------------------------------
  stratmeanData <- calc_stratified_mean(
    surveyData,
    areaPolygon,
    areaDescription,
    filterByArea,
    filterBySeason,
    groupDescription,
    filterByGroup,
    mergesexFlag,
    returnPrepData = T
  )
  # -----------------------------------------------------------------------
  # Calculate total biomass/abundance estimates
  # -----------------------------------------------------------------------
  message("Calculating Swept Area Estimate  ...")
  sweptareaData <- survdat::swept_area(
    prepData = stratmeanData$prepData,
    stratmeanData = stratmeanData$stratmeanData,
    q = q,
    areaDescription = areaDescription,
    a = a,
    groupDescription = groupDescription
  )
  # -----------------------------------------------------------------------
  # ISSUE 79 FIX: Explicitly assign units and format output
  # -----------------------------------------------------------------------
  if (tidy) {
    message("Tidying data  ...")
    # Converted to tidyr logic for safer execution and explicit unit mapping
    sweptareaData <- sweptareaData |>
      as_tibble() |>
      pivot_longer(
        cols = c(
          'strat.biomass',
          'biomass.var',
          'strat.abund',
          'abund.var',
          'tot.biomass',
          'tot.bio.var',
          'tot.abundance',
          'tot.abund.var'
        ),
        names_to = "variable",
        values_to = "value"
      ) |>
      mutate(
        units = case_when(
          variable == 'strat.biomass' ~ 'kg tow^-1',
          variable == 'biomass.var' ~ '(kg tow^-1)^2',
          variable == 'strat.abund' ~ 'number',
          variable == 'abund.var' ~ 'numbers^2',
          variable == 'tot.biomass' ~ 'kg',
          variable == 'tot.bio.var' ~ 'kg^2',
          variable == 'tot.abundance' ~ 'number',
          variable == 'tot.abund.var' ~ 'numbers^2',
          TRUE ~ NA_character_
        )
      )
  } else {
    # If not tidy, add descriptive unit columns to the wide format
    sweptareaData <- sweptareaData |>
      as_tibble() |>
      mutate(
        Biomass_Units = "kg",
        Abundance_Units = "number",
        Stratified_Biomass_Units = "kg tow^-1",
        Swept_Area_Used = a
      )
  }
  
  return(sweptareaData)
}
