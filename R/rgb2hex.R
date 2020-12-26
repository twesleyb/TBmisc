#' rgb2hex
#'
#' Convert a color's rgb representation to its hexadecimal code.
#'
#' @param r, g, b - numeric red, green, and blue components of a color
#'
#' @return hexadecimal code
#'
#' @author Hadley Wickam
#'
#' @references https://gist.github.com/mbannert/e9fcfa86de3b06068c83
#'
#' @keywords none
#'
#' @export
#'
rgb2hex <- function(r,g,b) { rgb(r, g, b, maxColorValue = 255) }
