gtabtheme <- function(){
	## FIXME: DOESNT WORK!
	# Modify default table theme to change font size.
	# Cex is a scaling factor relative to the defaults.
	mytheme <- gridExtra::ttheme_default({
		core = list(fg_params = list(cex = 0.75))
		colhead = list(fg_params = list(cex = 0.75))
		rowhead = list(fg_params = list(cex = 0.75))
	})
	return(mytheme)
}
