#' ggplotHistConnectivity
#'
#' Network connectivity histogram.
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
#' ggplotHistConnectivity(connectivity)
ggplotHistConnectivity <- function(connectivity) {
  plot <- qplot(connectivity,
    geom = "histogram",
    binwidth = 5,
    main = "Connectivity Histogram",
    xlab = "Connectivity (k)",
    ylab = "Frequency",
    fill = I("black"),
    col = I("black"),
    alpha = 0.2
  ) +
    scale_x_continuous(limits = c(0, 50), expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, color = "black", size = 14, face = "bold"),
      axis.title.x = element_text(color = "black", size = 11, face = "bold"),
      axis.title.y = element_text(color = "black", size = 11, face = "bold")
    )
  return(plot)
}
