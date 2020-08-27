#' random_color
#'
#' @export

random_color <-  function(count=1,hue=NULL,luminosity="light"){
       # Count - number of colors to generate.
       # Hue - a string 
       # luminosity - a string, one of c(light,dark,bright).
	
	# Path to python script.
	root <- getrd()
	script <- file.path(root,"Py","random_color.py")

	# Collect arguments in a list.
	args_list <- list("count"=count,"hue"=hue,
			  "luminosity"=luminosity)

	# Drop NULL args.
	drop <- which(sapply(args_list,is.null))
	if (length(drop) > 0) {
		args_list <- args_list[-drop]
	}

	# Format args as options passed to python script.
	options_ <- paste(paste(paste0("--",names(args_list)),args_list),
			  collapse=" ")

	# Evaluate call to random_color.py
	cmd <- paste(script, options_)
	response <- system(cmd,intern=TRUE)

	# Return the hex color(s).
	hex <- toupper(rjson::fromJSON(gsub("'","\"",response)))

	return(hex)
}
