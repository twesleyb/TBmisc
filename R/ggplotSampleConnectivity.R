#' ggplotSampleConnectivity
#'
#' Function for plotting sample connectivity.
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
#' @import ggplot2 WGCNA
#'
#' @export
#'
#' @examples
#' ggplotSampleConnectivity(data_in, log = TRUE, colID, threshold = -2.5)
ggplotSampleConnectivity <- function(data_in, log = TRUE, colID, threshold = -2.5) {
  require(ggdendro)
  require(WGCNA)
  require(ggrepel)
  cols <- grep(colID, colnames(data_in))
  dm <- as.matrix(data_in[, cols])
  if (log == TRUE) {
    dm <- log2(dm)
  } else {
    dm <- dm
  }
  # Calcualte adjacency matrix.
  adjm <- (0.5 + 0.5 * WGCNA::bicor(dm, use = "pairwise.complete.obs")^2)
  # Generate a dendrogram.
  sampleTree <- hclust(as.dist(1 - adjm), method = "average")
  dendro <- ggdendrogram(sampleTree, theme_dendro = FALSE) + xlab("Sample") +
    ylab("Height") + ggtitle("Sample Clustering to detect outliers") +
    theme(
      plot.title = element_text(hjust = 0.5, color = "black", size = 11, face = "bold"),
      axis.title.x = element_text(color = "black", size = 11, face = "bold"),
      axis.title.y = element_text(color = "black", size = 11, face = "bold")
    )
  # Calculate Ki (standardized connectivity)
  # The standardized connectivity (Z.K; Oldham et al.,) is a quantity that describes
  # the overall strength of connections between a given node (sample) and all of the other
  # nodes (samples) in a network.
  # The total connectivity of a Node (sample) is the sum of all of its connections (colSum).
  ki <- fundamentalNetworkConcepts(adjm)$Connectivity
  kmax <- max(ki)
  Ki <- ki / kmax # Normalized ki by maximum.
  Kmean <- mean(Ki)
  Kvar <- var(Ki)
  Z.Ki <- (Ki - Kmean) / sqrt(Kvar)
  # Gather data in df for plotting.
  data <- as.data.frame(Z.Ki)
  data$Sample <- sampleTree$order
  rownames(data) <- colnames(adjm)
  # Sort by standardized connectivity.
  data <- data[order(data$Z.Ki), ]
  data$label <- rownames(data)
  data$label[!data$Z.Ki < threshold] <- ""
  # Plot Z.K
  plot <- ggplot(data, aes(Sample, Z.Ki)) + geom_point(size = 2) + ggtitle("Sample Connectivity") +
    geom_hline(yintercept = threshold, linetype = "dashed", color = "red", size = 0.25) +
    geom_label_repel(aes(label = label), nudge_y = 0.3, colour = "red", alpha = 0.85) +
    theme(
      plot.title = element_text(hjust = 0.5, color = "black", size = 11, face = "bold"),
      axis.title.x = element_text(hjust = 0.5, color = "black", size = 11),
      axis.title.y = element_text(hjust = 0.5, color = "black", size = 11)
    )
  data$label <- NULL
  result <- list(data, dendro, plot)
  names(result) <- c("table", "dendrogram", "connectivityplot")
  return(result)
}
