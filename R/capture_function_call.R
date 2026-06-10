#' Captures function call
#'
#' A function call and its arguments, including default arguments, are captured and returned as class call
#'
#' @return the function call
#'
#'\@examples
#' \dontrun{
#' # Called internally in get_survdat_data, get_survdat_scallop_data, get_survdat_clam_data, and get_mass_inshore_survey_data
#' }
#'
#'@noRd

capture_function_call <- function(...) {
  call <- evalq(match.call(expand.dots = FALSE), parent.frame(1))
  formals <- evalq(formals(), parent.frame(1))

  for (i in setdiff(names(formals), names(call))) {
    call[i] <- list(formals[[i]])
  }

  return(match.call(sys.function(sys.parent()), call))
}
