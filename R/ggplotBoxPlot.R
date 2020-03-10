#' ggplotBoxPlot
#'
#' plot sample boxplots
#'
#' @param data_in - expression data.
#'
#' @return none
#'
#' @author Tyler W Bradshaw, \email{tyler.w.bradshaw@duke.edu}
#'
#' @references none
#'
#' @keywords none
#'
#' @import ggplot2
#'
#' @export
#'
#' @examples
#' ggplotBoxPlot(data_in, colors, title)
ggplotBoxPlot <- function(data_in, colID, colors, title) {
  # The function ggplotBoxPlot plots Run level boxplots with ggplot.
  data_in <- as.data.frame(data_in)
  idy <- grep(colID, colnames(data_in))
  dm <- as.matrix(data_in[, idy])
  data_temp <- melt(log2(dm))

  colnames(data_temp) <- c("Accession", "Run", "Intensity")
  data_temp$Run <- as.factor(data_temp$Run)
  data_temp <- na.omit(data_temp)

  # Discrete x-axis labels.
  # v <- seq(1,44,1)
  # v <- v[rep(c(FALSE,TRUE), 22)] <- ""

  plot <- ggplot(data_temp, aes(x = Run, y = Intensity, fill = Run)) +
    geom_boxplot(outlier.colour = "black", outlier.shape = 20, outlier.size = 1) +
    scale_fill_manual(
      values = colors,
      name = "Genotype",
      breaks = c(1, 12, 24, 35),
      labels = c("Syngap1", "Ube3a", "Shank2", "Shank3")
    ) +
    ggtitle(title) +
    xlab("TMT Run") +
    ylab("Log2 Intensity") +
    theme(
      plot.title = element_text(hjust = 0.5, color = "black", size = 11, face = "bold"),
      axis.title.x = element_text(color = "black", size = 11, face = "bold"),
      axis.text.x = element_text(color = "black", size = 8, angle = 45),
      axis.title.y = element_text(color = "black", size = 11, face = "bold")
    )

  return(plot)
}
