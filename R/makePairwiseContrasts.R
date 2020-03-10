#' makePairwiseContrasts
#'
#' Function for making pairwise contrasts.
#'
#' @param g1 - group1
#'
#' @param g2 - group2
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
#' makePairwiseContrasts(g1, g2, collapse = " - ")
makePairwiseContrasts <- function(g1, g2, collapse = " - ") {
  # Coerce to list if necessary.
  if (!inherits(g1, "list")) {
    g1 <- as.list(g1)
  }
  if (!inherits(g2, "list")) {
    g2 <- as.list(g2)
  }
  # Loop to generate pairwise contrasts.
  contrasts <- list()
  for (i in 1:length(g1)) {
    contrasts[[i]] <- expand.grid(g1[[i]], g2[[i]])
  }
  # Gather contrasts in a data matrix.
  contrasts <- do.call(rbind, contrasts)
  # If collapse = character, collapse.
  if (is.character(collapse)) {
    contrasts <- apply(contrasts, 1, function(x) paste(x, collapse = collapse))
  }
  return(contrasts)
}
