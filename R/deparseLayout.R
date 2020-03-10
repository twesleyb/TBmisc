#' deparseLayout
#'
#' Deparse RCy3 layout arguuments to be pased to LayoutNetwork()
#'
#' @param
#'
#' @return None
#'
#' @author Tyler W Bradshaw, \email{tyler.w.bradshaw@duke.edu}
#' @references \url{}
#' @keywords named list
#'
#' @examples
#' # Create a graph layout to be applied to the layers.
#' # To see all layout properties:
#' getLayoutPropertyNames("force-directed")
#' layout.name <- "force-directed"
#' layout.properties <- list(
#'   defaultEdgeWeight = 0,
#'   defaultSpringCoefficient = 0.0001,
#'   edgeAttribute = "weight",
#'   defaultNodeMass = 10,
#'   defaultSpringLength = 1,
#'   numIterations = 100
#' )
#' myLayout <- deparseLayout(layout.name, layout.properties)
deparseLayout <- function(layout.name, layout.properties) {
  myLayout <- paste(
    layout.name,
    paste(c(rbind(
      names(layout.properties), "=",
      unlist(layout.properties), " "
    )),
    collapse = ""
    )
  )
  return(myLayout)
}
