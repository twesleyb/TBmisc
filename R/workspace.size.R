#' Create an excel workbook.
#'
#' @return the size of the current environment in bytes
#'
#' @author sgibb
#'
#' @references \url{https://stackoverflow.com/questions/22023404}
#'
#' @export

workspace.size <- function() {
	ws <- sum(sapply(ls(envir=globalenv()), function(x) object.size(get(x))))
	class(ws) <- "object_size"
	print(ws)
}
