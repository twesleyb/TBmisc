#' prompt the user
#'
#' @return reads input from user
#'
#' @author the internet
#'
#' @export

prompt <- function(prompt = NULL) {
  # Default prompt.
  if (is.null(prompt)) {
    prompt <- "Press <Enter> to continue..."
  }
  if (interactive()) {
    invisible(readline(prompt))
  } else {
    cat(prompt)
    invisible(readLines(file("stdin"), 1))
  }
}
