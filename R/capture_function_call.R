#' Captures function call
#'
#' A function call and its arguments, including default arguments, are captured and returned as class call
#'
#' @return the function call
#'
#'
#'@noRd

capture_function_call <- function(...) {
  call <- evalq(match.call(expand.dots = FALSE), parent.frame(1))
  formals <- evalq(formals(), parent.frame(1))

  for(i in setdiff(names(formals), names(call))){
    call[i] <- list( formals[[i]] )
  }

  return(match.call(sys.function(sys.parent()), call))
}
