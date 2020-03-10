#' annotateTopTags
#'
#' A function to annotate DAP candidates.
#'
#' @param y_TT
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
#' annotateTopTags(y_TT)
annotateTopTags <- function(y_TT) {
  suppressPackageStartupMessages({
    require(AnnotationDbi)
    require(org.Mm.eg.db)
  })
  y_TT$logCPM <- 100 * (2^y_TT$logFC)
  colnames(y_TT)[2] <- "%WT"
  y_TT$candidate <- "no"
  y_TT[which(y_TT$FDR <= 0.10 & y_TT$FDR > 0.05), dim(y_TT)[2]] <- "low"
  y_TT[which(y_TT$FDR <= 0.05 & y_TT$FDR > 0.01), dim(y_TT)[2]] <- "med"
  y_TT[which(y_TT$FDR <= 0.01), dim(y_TT)[2]] <- "high"
  y_TT$candidate <- factor(y_TT$candidate, levels = c("high", "med", "low", "no"))
  return(y_TT)
}
