merge_partitions <- function(p1, p2) {
  p2 <- p2 + max(p1)
  idx <- match(names(p2), names(p1))
  p1[idx] <- p2
  return(p1)
}
