#!/usr/bin/env Rscript

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
#' @references none
#'
#' @keywords
#'
#' @examples
#' hex2col('3F273C')
#'
#' @export


fix_hex <- function(hex_color) {
	# Adds '#' to hex color code.
	# USAGE: fix_hex('3F273C')
	if (substr(hex_color,1,1) != "#") { 
		return(paste0("#",hex_color)) 
	} else {
		return(hex_color)
	}
}


hex2rgb <- function(hex,simplify=FALSE){
	# Coerce hex to rgb.
	# USAGE: hex2rgb('3F273C')
	rgb <- setNames(as.list(col2rgb(fix_hex(hex))),c("r","g","b"))
	if (simplify) {
		return(unlist(rgb)) 
	} else {
		return(rgb)
	}
}


rgb2col <- function(r,g,b,show=FALSE) {
	# Convert a rgb color to its approximate R color.
	suppressPackageStartupMessages({ library(scales) })
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
	# compare test colour with approximate matching colour
	# generates a plot saved as Rplot.pdf
	if (show) { scales::show_col(c(rgb2hex(r,g,b),rgb2hex(colourMap[approxMatchCol,2:4]))) }
	# return colour name
	return(approxMatchCol)
}


hex2col <- function(hex){
	# Wrapper that converts hex code to color name.
	library(argparser)
	library(scales)
	# Convert hex color to name.
	color <- do.call(rgb2col,as_rgb(hex))
	return(color)
}


if (!interactive()) { 
	# Parse input arguments.
	root <- getrd()
	renv::load(root,quiet=TRUE)
	ap <- argparser::arg_parser("Get the approximate name of a hexademical color.")
	ap <- argparser::add_argument(ap, "hex", help="the hex color to be converted", type="str")
	args <- argparser::parse_args(ap)
	hex <- args$hex
       	# Convert the color.
	hex <- parse_args() 
	message(hex2col(hex))
}
