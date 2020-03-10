#' ggsavePlots
#'
#' save a list of plots
#'
#' @param plots - list of plots
#'
#' @param filenames - filenames for saving plots
#'
#' @return None
#'
#' @author Tyler W Bradshaw, \email{tyler.w.bradshaw@duke.edu}
#'
#' @export
#'
#' @examples
ggsavePlots <- function(plots,filenames = NULL){
	suppressPackageStartupMessages({
		library(ggplot2)
	})
	if (is.null(filenames)) {
		# Insure that plot list has names.
		if (is.null(names(plots))) { 
			stop(paste("Please provide filenames as",
				   "argument to function or as names",
				   "of plots list."))
		filenames <- names(plots)
		}
	}
	# Loop to save plots.
	for (i in c(1:length(plots))){
		ggsave(filenames[i],plots[[i]])
	}
}
