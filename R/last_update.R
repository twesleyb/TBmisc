#' last.updated
#' date -r $FILENAME +"%Y-%m-%d"
#' @export last.updated

last.updated <- function(filepath) {
	# return date a file was last modified
	#filepath="~/projects/TBmiscR/README.md"
	stopifnot(file.exists(filepath))
	cmd <- paste("date -r", filepath, "+'%Y-%m-%d'")
	last_modified <- system(cmd, intern=TRUE)
	return(last_modified)
}

