#' Key of survey strata and corresponding EPUs
#'
#' Strata to EPU conversions derived from SOE_Workflows repository (https://github.com/NEFSC/READ_EDAB_SOE_Workflows/blob/main/R/aggregate_biomass.r#L118)
#'
#' @docType data
#' @name EPUstrata
#' @keywords datasets
#' @export
#' @format A tibble with 235 rows and 14 variables:
#' \describe{
#'   \item{\code{STRATUM}}{integer A numerical code which represents a survey stratum in the NEFSC Bottom Trawl Survey}
#'   \item{\code{EPU}}{charater An abbreviated name for the Ecological Production Unit (EPU) that corresponds to the STRATUM column}
#' }
"EPUstrata"
