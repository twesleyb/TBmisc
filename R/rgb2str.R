#' rgb2str
#'
#' get the approximate name of any rgb color.
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
#' @export

# Convert rgb to color name.
rgb2str <- function(r, g, b, show = FALSE) {
  # Convert a rgb color to its approximate R color.
  suppressPackageStartupMessages({
    library(scales)
  })
  # create colour name vs. rgb mapping table
  colourMap <- data.frame(colourNames = colours(), t(col2rgb(colours())))
  # input test colours
  testDF <- data.frame(colourNames = "testCol", red = r, green = g, blue = b)
  # combine both tables
  combDF <- rbind(testDF, colourMap)
  # convert in matrix representation
  combMat <- as.matrix(combDF[, -1])
  # add row labels as colour names
  rownames(combMat) <- combDF[, 1]
  # compute euclidean distance between test rgb vector and all the colours
  # from mapping table
  # using dist function compute distance matrix,retain only upper matrix
  # find minimum distance point from test vector
  # find closest matching colour name
  approxMatchCol <- which.min(as.matrix(dist(combMat, upper = TRUE))[1, ][-1])
  # return colour name
  return(names(approxMatchCol))
}
