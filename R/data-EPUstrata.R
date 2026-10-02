#' Key of survey strata and corresponding EPUs
#'
#' Strata to EPU mappings are defined in the [technical documentation](https://noaa-edab.github.io/tech-doc/epu.html) for the State of the Ecosystem report
#'
#' @docType data
#' @name EPUstrata
#' @keywords datasets
#' @format A tibble with 2 variables:
#' \describe{
#'   \item{\code{STRATUM}}{integer. A numerical code which represents a survey stratum in the NEFSC Bottom Trawl Survey}
#'   \item{\code{EPU}}{charater. An abbreviated name for the Ecological Production Unit (EPU) that corresponds to the STRATUM column}
#' }
"EPUstrata"
