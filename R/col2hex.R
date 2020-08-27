#' col2hex
#'
#' Convert a color's character string to its hexadecimal code.
#'
#' @param color - character
#'
#' @return none
#'
#' @author Tyler W Bradshaw, \email{tyler.w.bradshaw@duke.edu}
#'
#' @references none
#'
#' @keywords none
#'
#' @export
#'
#' @examples
#' col2hex('red', 204)
col2hex <- function(color, alpha = 255, maxValue = 255) {
  z <- grDevices::col2rgb(color)
  hex <- rgb(z[1], z[2], z[3], alpha = alpha, maxColorValue = maxValue)
  return(hex)
}
