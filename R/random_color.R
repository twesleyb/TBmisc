#' random_color
#'
#' @export

random_color <-  function(count=1,hue=NULL,
			  luminosity=c("bright","light","dark")){
	
	# Path to python script.
	root <- getrd()
	script <- file.path(root,"bin","random_color.py")

	# Collect arguments in a list.
	args_list <- list("count"=count,"hue"=hue,
			  "luminosity"=luminosity)

	# Drop NULL args.
	args_list <- args_list[-which(sapply(args_list,is.null))]

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



