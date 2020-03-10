filename <- function(name="Rfile",
		     file_ext=NULL,
		     output_directory=getwd(),
		     prefix = TRUE){
	# Generate a filename with prefix.
	file_name <- paste(name,file_ext,sep=".")
	file_path <- file.path(output_directory,file_name)
	full_name <- prefix_file(file_path)
	if (prefix) {
		return(full_name)
	} else {
		return(file_path)
}
}
