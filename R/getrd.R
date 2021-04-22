#' getrd - get a git repositories root directory
#'
#' get the root directory of a git project
#'
#' @param dpattern - directory pattern specifying root directory 
#' @param fpattern - file pattern specifying project's root directory
#' @param max_tries - maximum depth of project tree
#'
#' @return
#'
#' @author Tyler W Bradshaw, \email{twesleyb10@gmail.com}
#'
#' @references none
#'
#' @keywords
#'
#' @examples
#' getrd()
#' @export

getrd <- function(dpattern = ".git", fpattern = NULL, max_trys = 5) {
  here <- getwd()
  if (!is.null(dpattern) & is.null(fpattern)) {
    # loop to search for directory pattern
    root <- FALSE
    i <- 0
    while (!root & i < max_trys) {
      root <- dpattern %in% basename(list.dirs(here, recursive = FALSE))
      if (!root) {
        here <- dirname(here)
        i <- i + 1
      }
    }
  } else if (!is.null(fpattern) & is.null(dpattern)) {
    # loop to search for file pattern
    root <- FALSE
    i <- 0
    while (!root & i < max_trys) {
      root <- fpattern %in% list.files(here)
      if (!root) {
        here <- dirname(here)
        i <- i + 1
      }
    }
  } else {
    stop("Please provide a file pattern or directory pattern.")
  }
  # check if root was found
  if (root) {
    root_directory <- here
    return(root_directory)
  } else {
    stop(paste("Unable to find root directory after", max_trys, "."))
  }
}
