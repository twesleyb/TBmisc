#' ggplotVerboseBoxplot
#'
#' Generate WGCNA verbose boxplots
#'
#' @param x - ME vector
#' @param g - groups, same dimension as ME
#' @param contrasts - which groups to compare
#' @param order - order of the bars in the plot
#'
#' @return verbose boxplot
#'
#' @author Tyler W Bradshaw, \email{tyler.w.bradshaw@duke.edu}
#' @references \url{}
#' @keywords
#'
#' @export
#'
#' @examples
#' ggplotVerboseBoxplot(x, g, contrasts)
ggplotVerboseBoxplot <- function(x, groups, 
				 group_levels,control_group=NULL) {
  # Imports
  suppressPackageStartupMessages({
    require(FSA)
    require(ggplot2)
  })
  # Bind data together as a data.frame.
  df <- data.frame(x = x, g = groups[names(x)])
  # If plotting duplicated control data.
  if (!is.null(control_group)){
	  x0 <- subset(df,!grepl(control_group,df$g)) 
	  y0 <- subset(df,grepl(control_group,df$g)) 
	  z0 <- subset(df,grepl(control_group,df$g)) 
	  y0$g <- "WT.Cortex"
	  z0$g <- "WT.Striatum"
	  df <- rbind(x0,y0,z0)
  }
  # Define tissue type grouping for faceted plot.
  df$tissue <- ""
  df$tissue[grep("Cortex", df$g)] <- "Cortex"
  df$tissue[grep("Striatum", df$g)] <- "Striatum"
  # Coerce groups to factor.
  df$g <- factor(df$g,levels = group_levels)
  # Generate boxplot.
  plot <- ggplot(df, aes(x = g, y = x, group = g, fill = g)) + geom_boxplot() +
    geom_point(color = "white", size = 1, pch = 21, fill = "black") +
    ylab("Summary Expression") + xlab(NULL) +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, color = "black", size = 14, face = "bold"),
      axis.title.x = element_text(color = "black", size = 11, face = "bold"),
      axis.title.y = element_text(color = "black", size = 11, face = "bold"),
      axis.text.x = element_text(color = "black", angle = 45, hjust = 1)
    ) + facet_grid(. ~ tissue, scales = "free", space = "free") +
    theme(strip.text.x = element_text(size = 11, color = "black", face = "bold"))
  # Customize colors
  colors <- rep(c("gray", "#FFF200", "#00A2E8", "#22B14C", "#A349A4"), 2)
  plot <- plot + scale_fill_manual(values = colors)
  return(plot)
}
