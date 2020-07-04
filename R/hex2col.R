#' hex2col 
#'
#' get the approximate name of a hex color.
#'
#' @param hex - hexadecimal color
#'
#' @return color - name of the color
#'
#' @author Tyler W Bradshaw, \email{tyler.w.bradshaw@duke.edu}
#'
#' @references [Silence Dogood](https://bit.ly/3gngca8)
#'
#' @keywords
#'
#' @export hex2col

# Adds '#' to hex color code if its not there.
fix_hex <- function(hex_color) {
	if (substr(hex_color,1,1) != "#") { 
		return(paste0("#",hex_color)) 
	} else {
		return(hex_color)
	}
}

# Convert hex to rgb.
hex2rgb <- function(hex,simplify=FALSE){
	# Returns a list of r, g, and b components. 
	rgb <- setNames(as.list(col2rgb(fix_hex(hex))),c("r","g","b"))
	if (simplify) {
		return(unlist(rgb)) 
	} else {
		return(rgb)
	}
}

hex2col <- function(hex){
	# Converts hex -> rgb -> color name.
	suppressPackageStartupMessages({
		library(argparser)
		library(scales)
	})
       	# Convert the color.
	color <- names(do.call(rgb2col,hex2rgb(hex)))
	return(color)
}
