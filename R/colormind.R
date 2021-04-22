#' colormind
#'
#' Query the colormind API for a random color palette. Seed colors can be 
#' specified by passing up to five rgb colors to the function.
#'
#' @param c1 - a color passed as a rgb vector, e.g. c(85,90,84).
#' @param c2 - a color passed as a rgb vector, e.g. c(85,90,84).
#' @param c3 - a color passed as a rgb vector, e.g. c(85,90,84).
#' @param c4 - a color passed as a rgb vector, e.g. c(85,90,84).
#' @param c5 - a color passed as a rgb vector, e.g. c(85,90,84).
#'
#' @references \href{http://colormind.io/}{colormind}
#'
#' @import rjson
#'
#' @export

# Query colormind api for a random color palette
colormind <- function(c1=NA,c2=NA,c3=NA,c4=NA,c5=NA) {

	## DEFAULTS:
	curl_ = "curl 'http://colormind.io/api/'"
       	options_ =  "--silent" 
	data_ = "--data-binary"

	# Replace NA with 'N'.
	replace_NA <- function(x) { 
		if (inherits(x,"logical")) {
			return("N")
		} else {
			return(x)
		}
	}

	# If input color was not specified (is NA) then replace it with 'N'.
	input_colors <- lapply(list(c1,c2,c3,c4,c5),replace_NA)

	# Create input list
	input_list <- list("input" = input_colors,
			   "model" = "default")

	# Convert to json. Wrap in single quotes.
	input_json <- lquote({ rjson::toJSON(input_list) })

	# Create a cmd to query the colormind api with curl.
	cmd <- paste(curl_,options_,data_,input_json,collapse="")

	# Evaluate the command and parse the response as JSON.
	response <- system(cmd,intern=TRUE)
	result <- rjson::fromJSON(response)

	# Clean-up and return the colors.
	colors <- unlist(result,recursive=FALSE) # clean-up
	names(colors) <- gsub("result","color",names(colors))

	return(colors)
}
