source_funcdir <- function(funcdir) {
  myfun <- list.files(funcdir, pattern = "*.R", full.names = TRUE)
  invisible(sapply(myfun, source))
}
