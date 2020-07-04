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
#' @examples
#' hex2col('3F273C')
#' hex2col 3F273C # as executable
#'
#' @export

# Convert a rgb color to hex.
rgb2hex <- function(r,g,b) { rgb(r, g, b, maxColorValue = 255) }

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

# Convert rgb to color name.
rgb2col <- function(r,g,b,show=FALSE) {
	# Convert a rgb color to its approximate R color.
	# From:
	# https://stackoverflow.com/questions/41209395/from-hex-color-code-or-rgb-to-color-name-using-r
	suppressPackageStartupMessages({ 
		library(scales) 
	})
       	# create colour name vs. rgb mapping table 
	colourMap <- data.frame(colourNames = colours(),t(col2rgb(colours())))
	# input test colours
	testDF <- data.frame(colourNames="testCol",red = r,green = g,blue = b)
	# combine both tables
	combDF <- rbind(testDF,colourMap)
	# convert in matrix representation 
	combMat <- as.matrix(combDF[,-1])
	# add row labels as colour names
	rownames(combMat) <- combDF[,1]
	# compute euclidean distance between test rgb vector and all the colours
	# from mapping table 
	# using dist function compute distance matrix,retain only upper matrix
	# find minimum distance point from test vector
	# find closest matching colour name
	approxMatchCol <- which.min(as.matrix(dist(combMat,upper=TRUE))[1,][-1])
	# return colour name
	return(approxMatchCol)
}

hex2col <- function(hex){
	# Main function - a wrapper that converts hex -> rgb -> color name.
	suppressPackageStartupMessages({
		library(argparser)
		library(scales)
	})
       	# Convert the color.
	color <- names(do.call(rgb2col,hex2rgb(hex)))
	return(color)
}
