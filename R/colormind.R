#' colormind
#'
#' @export

# Query colormind api for random colors.
colormind <- function(c1=NA,c2=NA,c3=NA,c4=NA,c5=NA,random=TRUE) {

	# Replace NA with 'N'.
	replace_NA <- function(x) { 
		if (is.na(x)) {
			return("N")
		} else {
			return(x)
		}
	}

	# If input color was not specified (is NA) then replace it with 'N'.
	input_colors <- sapply(c(c1,c2,c3,c4,c5),replace_NA)

	# Query the colormind api with curl.
	curl_ = "curl 'http://colormind.io/api/'"
       	options_ =  "--silent" 
	data_ = "--data-binary"

	# Create input list
	input_list <- list("input" = input_colors,
			   "model" = "default")

	# Convert to json. Wrap in single quotes.
	input_json <- lquote({ rjson::toJSON(input_list) })

	# Create system cmd.
	cmd <- paste(curl_,options_,data_,input_json,collapse="")

	# Evaluate the command and parse the response as JSON.
	response <- system(cmd,intern=TRUE)
	result <- rjson::fromJSON(response)

	# Clean-up and return the colors.
	colors <- unlist(result,recursive=FALSE) # clean-up
	names(colors) <- gsub("result","color",names(colors))

	return(colors)
}
