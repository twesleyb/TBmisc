#' rgb2col 
#'
#' get the approximate name of a rgb color.
#'
#' @param r - red
#'
#' @param g - green
#'
#' @param b - blue
#'
#' @return color - name of the color
#'
#' @author Tyler W Bradshaw, \email{tyler.w.bradshaw@duke.edu}
#'
#' @references [Silence Dogood](https://bit.ly/3gngca8)
#'
#' @keywords
#'
#' @export rgb2col

# Convert a rgb color to hex.
rgb2hex <- function(r,g,b) { rgb(r, g, b, maxColorValue = 255) }

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
