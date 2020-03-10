#' ggplotProteinBoxPlot
#'
#' Function to make boxplots for proteins of interst.
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
#' @import ggplot2
#'
#' @export
#'
#' @examples
#' ggplotProteinBoxPlot(data_in, interesting.proteins, traits, scatter = TRUE)
ggplotProteinBoxPlot <- function(data_in,
                                 interesting.proteins,
                                 traits,
                                 box_order = NULL,
                                 scatter = FALSE) {
  proteinBoxes <- list()
  for (protein in interesting.proteins) {
    # Subset the data.
    idx <- match(protein, rownames(data_in))
    data_sub <- data.frame("Intensity" = data_in[idx, ])
    data_sub$Group <- traits$Condition[match(
      rownames(data_sub),
      rownames(traits)
    )]
    data_sub <- na.omit(data_sub)
    # If provided, enforce order.
    if (is.character(box_order)) {
      data_sub <- subset(data_sub, data_sub$Group %in% box_order)
      data_sub$Group <- factor(data_sub$Group, levels = box_order)
    } else {
      data_sub$Group <- factor(data_sub$Group, levels = unique(data_sub$Group))
    }
    # Add tissue type grouping.
    data_sub$tissue <- rep(NA, nrow(data_sub))
    data_sub$tissue[grepl("Cortex", data_sub$Group)] <- "Cortex"
    data_sub$tissue[grepl("Striatum", data_sub$Group)] <- "Striatum"
    # Generate plot.
    plot <- ggplot(data_sub, aes(x = Group, y = Intensity, fill = Group)) +
      geom_boxplot(
        outlier.colour = "black",
        outlier.shape = 20, outlier.size = 1
      ) +
      ggtitle(protein) + ylab("Log2(Relative Abundance)") + xlab("") +
      theme(
        plot.title = element_text(
          hjust = 0.5, color = "black",
          size = 11, face = "bold"
        ),
        axis.title.x = element_text(color = "black", size = 10),
        axis.title.y = element_text(color = "black", size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"
      )
    # Add scatter
    if (scatter == TRUE) {
      plot <- plot + geom_point(aes(x = Group, y = Intensity), color = "white", size = 2, pch = 21, fill = "black")
    }
    # facet wrap.
    # plot <- plot + facet_grid(. ~ tissue, scales = "free", space = "free")
    # Store in list.
    proteinBoxes[[protein]] <- plot
  }
  return(proteinBoxes)
}
