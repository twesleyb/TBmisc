#' annotate_stars
#'
#' Function to add significance stars given a protein boxplot,
#' stats with FDR column and the column to be labeled.
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
#' annotate_stars(plot, stats)
annotate_stars <- function(plot, stats) {
  suppressPackageStartupMessages({
    require(dplyr)
  })

  data <- plot$data
  df <- as.data.frame(data %>% group_by(Group) %>%
    dplyr::summarise(Intensity = mean(Intensity)))

  # Get FDR from stats.
  plot_name <- plot$labels$title
  idx <- match(plot_name, rownames(stats))
  stats.df <- t(stats[idx, ])
  df$FDR <- stats.df[match(df$Group, rownames(stats.df)), ]
  df$FDR[is.na(df$FDR)] <- 1
  # Add symbols.
  df$symbol <- ""
  df$symbol[df$FDR < 0.05] <- "*"
  df$symbol[df$FDR < 0.005] <- "**"
  df$symbol[df$FDR < 0.0005] <- "***"
  # Add ypos.
  df$ypos <- 1.01 * max(data$Intensity)
  # Text color.
  df$color <- rep("black", nrow(df))
  df$color[df$FDR < 0.05] <- "red"

  # Add asterisks indicating significance to plot.
  plot <- plot + annotate("text",
    x = df$Group, y = df$ypos,
    label = df$symbol, size = 5
  )

  # Add red text if significant.
  plot <- plot +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = df$color))
  return(plot)
}
