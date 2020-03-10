files.no.dirs <- function(path=getwd()){
	# list files but not directories in the provided path.
	# Author: Sven Hohenstein.
	# REF: https://stackoverflow.com/questions/22069095/r-get-list-of-files-but-not-of-directories
	files <- list.files(path,full.names=TRUE)
	dirs <- list.dirs(path,full.names=TRUE,recursive = FALSE)
	return(setdiff(files,dirs))
}
