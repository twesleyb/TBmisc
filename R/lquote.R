#' lquote
#' @export
lquote <- function(string,single=TRUE){
	# Wrap a string in single or double quotes.
	single_quote = "'"
	double_quote = "\""
	if (single) {
		# Single quote.
		return(paste0(single_quote,string,single_quote))
	} else {
		# Double quote.
		return(paste0(double_quote,string,double_quote))
	}
}
