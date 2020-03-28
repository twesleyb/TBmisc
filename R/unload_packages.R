#' unload_packages
#'
#' A function for detaching packages. 
#'
#' @param all - should all packages be unloaded?
#'
#' @return none
#'
#' @author Tyler W Bradshaw, \email{tyler.w.bradshaw@duke.edu}
#'
#' @references none \link{https://stackoverflow.com/questions/7505547/detach-all-packages-while-working-in-r}
#'
#' @keywords none
#'
#' @export
#'
#' @examples
#' detach_packages()
detach_packages <- function(pkg, all=FALSE) {
	if (all == TRUE){
		pkgs <- names(sessionInfo()$otherPkgs)
	} else {
		pkgs <- as.list(pkg)
	}
	invisible(lapply(paste0('package:',pkgs),
			 detach, character.only=TRUE, unload=TRUE, force=TRUE))
}

