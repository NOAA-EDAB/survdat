#' Argument Flag check
#'
#' check to make sure arguments passed do not contradict each other
#' @inheritParams get_survdat_data
#'
#' @noRd

# We can either create error messages (like below) or fix argument specs
check_argument_validation <- function(getLengths,getBio,getWeightLength) {

  if (getWeightLength) {
    if(!getLengths) {
      # Weight at Length Data -----------------------------------------------------
      stop("Can not calculate weight at length without lengths...
           Set getLengths = TRUE.")
    }
  }

  if (getBio) {
    if(!getLengths) {
      stop("Can not obtain individual fish biological data without lengths ...
           Set getLengths = TRUE")
    }
  }


}
