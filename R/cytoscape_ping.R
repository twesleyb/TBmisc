cytoscape_ping <- function() {
	suppressPackageStartupMessages({
		library(RCy3)
	})
	tryCatch(
	 expr = {
		 cytoscapePing()
	 },
	 error = function(e) {
		 prompt <- paste("Please open Cytoscape.",
				 "When ready to proceed,",
				 "press [enter] to continue...\n")
		 # Prompt the user to open cytoscape.
		 if (interactive()) {
			 invisible(readline(prompt))
		 } else {
			 cat(prompt)
			 invisible(readLines(file("stdin"), 1))
		 }
	 },
	 finally = {
		 cytoscapePing()
	 }
	 )
}
