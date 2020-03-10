#' annotate_plot
#'
#' Function to add significance stars given a protein boxplot and stats.
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
#' annotate_plot(plot, stats)
annotate_plot <- function(plot, stats) {
  # Annotate boxplot with significance stars.
  # Simplify x axis text.
  # Facet.
  # Collect plots data.
  data <- plot$data
  # Build annotation df.
  df <- data %>%
    group_by(Group) %>%
    summarize("Intensity" = mean(Intensity))
  df$Tissue <- sapply(strsplit(as.character(df$Group),"\\."),"[",1)      
  df$Genotype <- sapply(strsplit(as.character(df$Group),"\\."),"[",3)      
  df$Genotype[is.na(df$Genotype)] <- "WT"
  # Get FDR.
  fdr <- as.numeric(subset(stats, rownames(stats) == plot$labels$title))
  names(fdr) <- gsub("FDR\\.","",colnames(stats))
  df$fdr <- fdr[match(paste(df$Genotype,df$Tissue),names(fdr))]
  df$fdr[is.na(df$fdr)] <- 1
  df$ypos <- 1.01 * max(df$Intensity)
  df$symbol <- ""
  df$symbol[df$fdr < 0.05] <- "*"
  df$symbol[df$fdr < 0.005] <- "**"
  df$symbol[df$fdr < 0.0005] <- "***"
  df$tissue <- sapply(strsplit(as.character(df$Group), "\\."), "[", 1)
  df$color <- rep("black", nrow(df))
  df$color[df$fdr < 0.05] <- "red"
  # Generate faceted plot.
  plot <- plot + geom_text(data = df, aes(x = Group, y = ypos, label = symbol), size = 6) +
    # theme(axis.text.x = element_text(colour = df$color))
    facet_grid(. ~ tissue, scales = "free", space = "free")
  # Simplify x.axis names.
  plot <- plot +
    scale_x_discrete(labels = rep(c("WT", "Shank2", "Shank3", "Syngap1", "Ube3a"), 2))
  # Modify facet labels.
  plot <- plot + theme(strip.text.x = element_text(size = 11, color = "black", face = "bold"))
  return(plot)
}
