#' @export
load_all <- function(funcdir = file.path(getrd(),"R")) {
		myfun <- list.files(funcdir, pattern="*\\.R", full.names=TRUE)
		msg <- do.call(file.path,
			       as.list(tail(unlist(strsplit(funcdir,"\\/")),2)))
		message(paste("Loading functions in", msg))
		invisible({ sapply(myfun,source) })
}
