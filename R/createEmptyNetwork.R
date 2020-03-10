# Create a new network collection in Cytoscape.
createEmptyNetwork <- function(name="Network") {
	suppressPackageStartupMessages({
		library(RCy3)
	})
	cmd <- paste("network create empty name=",name)
	commandsPOST(cmd)
}
