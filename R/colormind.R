#' colormind
#'
#' @export

# Query colormind api for random colors.
colormind <- function(random=TRUE) {
	# Query api:
	cmd <- "curl 'http://colormind.io/api/' --silent --data-binary '{\"model\":\"default\"}'"
	response <- system(cmd,intern=TRUE)
	# Parse the response as JSON.
	result <- rjson::fromJSON(response)
	colors <- unlist(result,recursive=FALSE) # clean-up
	names(colors) <- gsub("result","color",names(colors))
	return(colors)
}
