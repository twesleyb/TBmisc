#' ggplotPCA
#'
#' plot a PCA plot with ggplot
#'
#' @param data_in - the expression data
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
#' ggplotPCA(data_in, traits, colors)
ggplotPCA <- function(data_in, traits, colors, colID = "Abundance", title = "2D PCA Plot") {
  # Get the numeric data.

  data_in <- as.data.frame(data_in)
  idy <- grepl(colID, colnames(data_in))
  data_in <- data_in[, idy]
  # Perform PCA. Must remove any NA.
  PC <- prcomp(t(na.omit(data_in)))$x[, 1:2]
  # Add annotations to PC data frame.
  PC <- as.data.frame(PC)
  idx <- match(rownames(PC), traits$ColumnName)
  PC$Label <- paste(traits$Model, traits$SampleType, sep = "_")[idx]

  # Generate plot.
  plot <- ggplot(PC, aes(x = PC1, y = PC2)) +
    geom_text(aes(label = Label), color = colors) +
    ggtitle(title) +
    theme(
      plot.title = element_text(hjust = 0.5, color = "black", size = 11, face = "bold"),
      axis.title.x = element_text(color = "black", size = 11, face = "bold"),
      axis.title.y = element_text(color = "black", size = 11, face = "bold")
    )
  return(plot)
}
