#' ntile
#'
#' Create groups where the groups each have as close to the same number of
#' members as possible.
#'
#' @param x
#'
#' @param ngroups
#'
#' @param na.rm
#'
#' @param result
#'
#' @param reverse
#'
#' @param checkBleed
#'
#' @return none
#'
#' @author Tyler W Bradshaw, \email{tyler.w.bradshaw@duke.edu}
#'
#' @references
#' \href{https://www.rdocumentation.org/packages/BurStMisc/versions/1.1/topics/ntile}
#'
#' @keywords bin groups distribution
#'
#' @export
#'
#' @examples
#' ntile(setNames(state.area, state.name), 10)
ntile <- function(x, ngroups, na.rm = FALSE, result = "list", reverse = FALSE,
                  checkBleed = TRUE) {
  stopifnot(is.numeric(ngroups), length(ngroups) == 1, ngroups >
    0)
  result.menu <- c("list", "numeric", "factor")
  result.num <- pmatch(result, result.menu, nomatch = 0)
  if (result.num == 0L) {
    stop(
      "'result' must be (an abbreviation of) one of: ",
      paste(result.menu, collapse = ", ")
    )
  }
  result <- result.menu[result.num]
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  else if (nas <- sum(is.na(x))) {
    stop(nas, " missing values present")
  }
  nx <- length(x)
  if (nx < ngroups) {
    stop(
      "more groups (", ngroups, ") than observations (",
      nx, ")"
    )
  }
  basenum <- nx %/% ngroups
  extra <- nx %% ngroups
  repnum <- rep(basenum, ngroups)
  if (extra) {
    eloc <- seq(floor((ngroups - extra) / 2 + 1), length = extra)
    repnum[eloc] <- repnum[eloc] + 1
  }
  if (reverse) {
    groupvec <- rep(ngroups:1, rev(repnum))[order(order(x))]
  }
  else {
    groupvec <- rep(1:ngroups, repnum)[order(order(x))]
  }
  names(groupvec) <- names(x)
  grouplist <- split(x, groupvec)
  if (checkBleed && ngroups > 1) {
    bleeding <- rep(FALSE, ngroups)
    if (reverse) {
      for (i in 2:ngroups) {
        if (max(grouplist[[i]]) >= min(grouplist[[i -
          1L]])) {
          bleeding[(i - 1L):i] <- TRUE
        }
      }
    }
    else {
      for (i in 2:ngroups) {
        if (max(grouplist[[i - 1L]]) >= min(grouplist[[i]])) {
          bleeding[(i - 1L):i] <- TRUE
        }
      }
    }
    if (any(bleeding)) {
      warning("common values across groups: ", paste(which(bleeding),
        collapse = ", "
      ))
    }
  }
  switch(result, list = {
    grouplist
  }, numeric = {
    groupvec
  }, factor = {
    ordered(groupvec, levels = if (reverse) ngroups:1 else 1:ngroups)
  })
}
