#' named.list
#'
#' Create a named list.
#'
#' @param
#'
#' @return None
#'
#' @author Tyler W Bradshaw, \email{tyler.w.bradshaw@duke.edu}
#' @references \url{https://stackoverflow.com/questions/17831015/r-list-from-variables-using-variable-names}
#' @keywords named list
#'
#' @examples
#' named.list()
named.list <- function(...) {
  l <- list(...)
  names(l) <- sapply(substitute(list(...)), deparse)[-1]
  return(l)
}
