#' Length-weight parameters from Wigley et al. (2003)
#'
#' Length-weight relationships for 74 fish species collected during NEFSC
#' research vessel bottom trawl surveys, 1992-99 as published in Wigley et al. (2003)
#'
#' @docType data
#' @name wigley_lw
#' @keywords datasets
#' @export
#' @format A data frame with 235 rows and 14 variables:
#' \describe{
#'   \item{\code{SpeciesName}}{character Species common name}
#'   \item{\code{LW_SVSPP}}{integer A standard code which represents a species caught in a trawl or dredge. Refer to the SVDBS.SVSPECIES_LIST}
#'   \item{\code{Season}}{character Season sampled (Winter, Autumn, Spring or a combination)}
#'   \item{\code{Gender}}{character Gender of the fish sampled (Female, Male, or Combined)}
#'   \item{\code{N}}{integer Number of fish sampled}
#'   \item{\code{ln_a}}{character Length-weight parameter estimate}
#'   \item{\code{SEa}}{double Standard error of ln a}
#'   \item{\code{b}}{double Length-weight parameter estimate}
#'   \item{\code{SEb}}{double Standard error of b}
#'   \item{\code{SE_estimate}}{double Standard error of the weight estimate}
#'   \item{\code{r2}}{double Regression correlation coefficient}
#'   \item{\code{lna1}}{double Length-weight parameter estimate}
#'   \item{\code{lna}}{double Inverse of ln_a and lna1}
#'   \item{\code{SEASON}}{character Parsed season data into Winter, Spring, or Fall}
#' }
"Wigley_LW"
