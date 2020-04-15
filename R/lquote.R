#' lquote
#' @export
lquote <- function(string,single=TRUE){
	# Wrap a string in single or double quotes.
	single_quote = "'"
	double_quote = "\""
	if (single) {
		return(paste0(strwrap(string,prefix=single_quote),single_quote))
	} else {
		double = TRUE
	if (double) { paste0(strwrap(string,prefix=double_quote),double_quote) }
	}
}
