#' hex2name
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
#' @export

hex2name <- function(hex) {
  # Converts hex -> rgb -> color name.
  suppressPackageStartupMessages({
    library(scales)
  })
  # If necessary, fix hex color (add #).
  if (substr(hex, 1, 1) != "#") {
    hex <- paste0("#", hex)
  }
  # Convert hex to rgb.
  rgb <- setNames(as.list(col2rgb(fix_hex(hex))), c("r", "g", "b"))
  ## Get nearest neighbor in color space.
  # create colour name vs. rgb mapping table
  colourMap <- data.frame(colourNames = colours(), t(col2rgb(colours())))
  # input test colours
  testDF <- data.frame(colourNames = "testCol", red = rgb$r, green = rgb$g, blue = rgb$b)
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
