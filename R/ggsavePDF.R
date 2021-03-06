#' ggsavePDF
#'
#' Function for saving multiple ggplots to single pdf.
#'
#' @return none
#'
#' @author Tyler W Bradshaw, \email{twesleyb10@gmail.com}
#'
#' @references none
#'
#' @keywords none
#'
#' @examples
#' ggsavePDF(plots, file)
ggsavePDF <- function(plots, file) {
  library(ggplotify)
  pdf(file, onefile = TRUE)
  # if not a list, coerce to list
  if (!inherits(plots, "list")) {
    plot_list <- list(plots)
  } else {
    plot_list <- plots
  }
  # loop through list, save plots to pdf
  for (i in 1:length(plot_list)) {
    # if ggplot, then print
    plot <- plot_list[[i]]
    if (inherits(plot, "ggplot")) {
      print(plot)
      # if else, coerce to ggplot and print
    } else {
      print(as.ggplot(plot))
    }
  }
  silence(dev.off())
}
