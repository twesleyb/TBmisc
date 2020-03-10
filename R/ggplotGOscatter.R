#' ggplotGOscatter
#'
#' Function for visualizing GO terms.
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
#' @import
#'
#' @export
#'
#' @examples
#' ggplotGOscatter(GO_results, color, topN = 10)
ggplotGOscatter <- function(GO_results, color, topN = 15) {
	suppressPackageStartupMessages({
		library(data.table)
		library(ggplot2)
		library(ggrepel)
	})
  # Collect data in df.
  df <- data.table(
    x = GO_results$enrichmentRatio,
    y = -log(GO_results$pValue),
    pValue = as.numeric(GO_results$pValue),
    FDR = as.numeric(GO_results$FDR),
    pAdj = as.numeric(GO_results$Bonferroni),
    nGenes = as.numeric(GO_results$nCommonGenes),
    label = as.character(GO_results$shortDataSetName)
  )
  df$score <- df$x * df$y
  df <- df[order(df$score, decreasing = TRUE), ]
  # Display only the topN terms.
  if (dim(df)[1] + 1 < topN) {
    topN <- dim(df)[1] - 1
  }
  df$label[seq(topN + 1, nrow(df))] <- ""
  # Generate plot.
  plot <- ggplot(df, aes(
    x = x, y = y, colour = FDR,
    size = nGenes, label = label
  )) +
    geom_point() + geom_text_repel(colour = "black", alpha = 0.85) +
    scale_colour_gradient(low = color, high = "white") +
    xlab("Fold Enrichment") +
    ylab("-Log(P-value)") +
    ggtitle("Go Enrichment") +
    theme(
      plot.title = element_text(hjust = 0.5, color = "black", size = 8, face = "bold"),
      axis.title.x = element_text(color = "black", size = 8, face = "bold"),
      axis.title.y = element_text(color = "black", size = 8, face = "bold")
    )
  return(plot)
}
