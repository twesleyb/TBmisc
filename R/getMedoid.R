#' getMedoid
#'
#' Find representative branch from groups determined by heirarchical clustering
#'
#' @param adjm adjacency matrix for heirarchical clustering
#'
#' @return none
#'
#' @author Tyler W Bradshaw, \email{tyler.w.bradshaw@duke.edu}
#'
#' @references none
#'
#' @keywords none
#'
#' @export
#'
#' @examples
#' getMedoid(adjm, h)
getMedoid <- function(adjm, k = NULL, h = NULL, method = "ward.D2") {
  # The medoid of the group is the branch that is closest to
  # all branches in its group.
  hc <- hclust(as.dist(1 - adjm), method)
  partition <- cutree(hc, k, h)
  groups <- split(partition, partition)
  # Remove groups of length 1.
  out <- which(sapply(groups, length) == 1)
  if (length(out) > 0) {
    removed <- sapply(groups[out], function(x) names(x)[1])
    groups <- groups[-out]
  } else {
    removed <- NULL
  }
  # Get groups of length 1.
  out2 <- which(sapply(groups, length) == 2)
  if (length(out2) > 0) {
    removed2 <- sapply(groups[out2], function(x) names(x)[2])
    groups <- groups[-out2]
  } else {
    removed2 <- NULL
  }
  # Get Medoid.
  if (length(groups) > 0) {
    rep_modules <- sapply(groups, function(x) {
      col_sums <- apply(adjm[names(x), names(x)], 2, sum)
      idy <- which(col_sums == min(col_sums))
      if (length(idy) > 1) {
        idy <- idy[length(idy)]
        }
      return(names(col_sums)[idy])
      })
  } else { 
    rep_modules = NULL
  }
  rep_branches <- c(removed, removed2, rep_modules)
  return(rep_branches[order(names(rep_branches))])
}