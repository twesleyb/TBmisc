
get_help <- function(fun, pkg = NULL) {
	# Define a function to parse the help documentation of an R
	# function.
	# From StackOverflow:
	# https://stackoverflow.com/questions/8918753/r-help-page-as-object
	if (class(pkg) == "NULL") {
		help_file <- utils:::.getHelpFile(help(eval(fun)))
	} else if (class(pkg) == "character") {
		help_file <- utils:::.getHelpFile(help(eval(fun), eval(pkg)))
	} else {
		error("pkg should be a character string.")
	}
	help_latex <- capture.output({
		tools:::Rd2latex(help_file)
	})
	# help_text <- tools::parseLatex(help_latex)
	return(help_latex)
}
