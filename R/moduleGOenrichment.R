#' moduleGOenrichment
#'
#' A function to perform GO enrichmnet analysis of protein co-expression
#' modules.
#'
#' @param
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
#' moduleGOenrichment()
moduleGOenrichment <- function(partitions, resolution, protmap, GOcollection,
                               exclude = "0") {
  # Function to perform GO enrichment for all modules in a given partition.
  suppressPackageStartupMessages({
    library(org.Mm.eg.db)
    library(anRichment)
  })
  # Create a matrix of module labels to be passed to anRichment.
  part <- partitions[[resolution]]
  modules <- split(part, part)
  modules <- modules[names(modules)[names(modules) != exclude]]
  classLabels <- sapply(names(modules), function(x) part == x)
  colnames(classLabels) <- paste0("R", resolution, "-M", names(modules))
  logic <- classLabels == TRUE
  for (i in 1:ncol(classLabels)) {
    col_header <- colnames(classLabels)[i]
    classLabels[logic[, i], i] <- col_header
    classLabels[!logic[, i], i] <- "NA"
  }
  classLabels <- classLabels[!duplicated(rownames(classLabels)), ]
  # Map protein ids to to entrez.
  entrez <- protmap$entrez[match(rownames(classLabels), protmap$ids)]
  rownames(classLabels) <- entrez
  # Perform GO enrichment.
  GOenrichment <- enrichmentAnalysis(
    classLabels,
    identifiers = entrez,
    refCollection = GOcollection,
    active = NULL,
    inactive = NULL,
    useBackground = "given",
    threshold = 0.05,
    thresholdType = "Bonferroni",
    getOverlapEntrez = TRUE,
    getOverlapSymbols = FALSE,
    ignoreLabels = "NA",
    verbose = 0
  )
  # Collect the results.
  GO_results <- list()
  for (r in 1:length(GOenrichment$setResults)) {
    GO_results[[r]] <- GOenrichment$setResults[[r]]$enrichmentTable
  }
  names(GO_results) <- colnames(classLabels)
  # Return results.
  return(GO_results)
} # Ends function.
