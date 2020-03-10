#' prefix_file
#' add a prefix to filename
prefix_file <- function(file_path, file_name = NULL, output_dir = NULL, 
			width = 3,date=FALSE) {
  # Profide either a full path to a file,
  # or a directory.
	if (is.path(file_path)) {
		output_dir <- dirname(file_path)
	} else {
		output_dir <- getwd()
	}
  indexed_files <- list.files(output_dir, pattern = ("[0-9]{2,4}_"))
  if (length(indexed_files) == 0) {
    last_file <- 0
  } else {
    last_file <- max(as.numeric(sapply(strsplit(indexed_files, "_"), "[", 1)))
  }
  index <- formatC(last_file + 1, width, format = "d", flag = "0")
  if (date) { 
	  prefix <- paste(index, Sys.Date(), sep = "_")
	  output_file <- file.path(output_dir, 
				   paste(prefix, basename(file_path), sep = "_"))
  } else if (!date) {
	  output_file <- file.path(output_dir, 
				   paste(index, basename(file_path), sep = "_"))
  }
  return(output_file)
}
