#' ggplotMDS
#'
#' Generate MDS plot with ggplot.
#' Does not log transform the data.
#'
#' @param data_in - expression data
#'
#' @return none
#'
#' @author Tyler W Bradshaw, \email{tyler.w.bradshaw@duke.edu}
#'
#' @references none
#'
#' @keywords none
#'
#' @import ggplot2 ggdendro
#'
#' @export
#'
#' @examples
#' ggplotMDS(data_in, colID, traits, title)
ggplotMDS <- function(data_in, colID, traits, title) {
  suppressPackageStartupMessages({
    require(ggdendro)
  })
  idx <- match(colnames(data_in), rownames(traits))
  # colnames(data_in) <- traits$SampleType[idx]
  colnames(data_in) <- traits$Sample.Model[idx]
  # colnames(data_in) <- traits$PrepDate[idx]
  ff <- tempfile()
  png(filename = ff)
  data_MDS <- limma::plotMDS(data_in)
  x <- data_MDS$x
  y <- data_MDS$y
  dev.off()
  unlink(ff)
  dm_MDS <- cbind(x, y)
  Condition <- rownames(dm_MDS)
  df_MDS <- as.data.frame(cbind(x, y))
  plot <- ggplot(df_MDS, aes(x, y, color = Condition)) + geom_text(aes(label = Condition), size = 6) +
    ggtitle(title) + xlab("Leading LogFC dim 1") + ylab("Leading LogFC dim 2") +
    theme(
      legend.position = "right",
      plot.title = element_text(hjust = 0.5, color = "black", size = 11, face = "bold"),
      axis.title.x = element_text(color = "black", size = 11, face = "bold"),
      axis.title.y = element_text(color = "black", size = 11, face = "bold")
    )
  # Dendrogram
  distMat <- dist(data_MDS$cmdscale.out)
  sampleTree <- flashClust(distMat, method = "complete")
  dendro <- ggdendrogram(sampleTree, rotate = TRUE, theme_dendro = FALSE) + xlab("Sample") +
    ylab("Height") + ggtitle("Sample Clustering based on MDS") +
    theme(
      plot.title = element_text(hjust = 0.5, color = "black", size = 11, face = "bold"),
      axis.title.x = element_text(color = "black", size = 11, face = "bold"),
      axis.title.y = element_text(color = "black", size = 11, face = "bold")
    )
  data_out <- list(data_MDS, plot, dendro)
  names(data_out) <- c("data_MDS", "plot", "dendro")
  return(data_out)
}
