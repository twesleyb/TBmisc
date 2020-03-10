#' ggplotProteinScatterPlot
#'
#' Generate a scatter plot between two proteins.
#'
#' @param none
#'
#' @return None
#'
#' @author Tyler W Bradshaw, \email{tyler.w.bradshaw@duke.edu}
#'
#' @references \url{}
#'
#' @keywords
#'
#' @examples
#' ggplotProteinScatterPlot(cleanDat, prot1, prot2)
#' @export
ggplotProteinScatterPlot <- function(cleanDat, prot1, prot2) {

  # collect data in dataframe; colnames are protein/gene names.
  df <- data.frame(
    prot1 = cleanDat[, colnames(cleanDat) == prot1],
    prot2 = cleanDat[, colnames(cleanDat) == prot2]
  )

  # Calculate best fit line and bicor stats.
  fit <- lm(df$prot2 ~ df$prot1)
  stats <- unlist(WGCNA::bicorAndPvalue(df$prot1, df$prot2))

  # Generate a descriptive title.
  mytitle <- paste0(
    "R² = ", round(stats["bicor"], 2),
    ", P = ", formatC(stats["p"], format = "e", digits = 2)
  )

  # Create the plot.
  plot <- ggplot(df, aes(x = prot1, y = prot2)) +
    geom_point(color = "white", pch = 21, fill = "black", size = 2) +
    geom_abline(
      intercept = coef(fit)[1], slope = coef(fit)[2],
      color = "black", linetype = "dashed"
    ) +
    ggtitle(mytitle) +
    xlab(paste("Log₂(Expression ", strsplit(prot1, "\\|")[[1]][1], ")", sep = "")) +
    ylab(paste("Log₂(Expression ", strsplit(prot2, "\\|")[[1]][1], ")", sep = "")) +
    ggtheme()
  return(plot)
}
