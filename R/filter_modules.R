#' filter_modules
#'
#' remove modules less than a minimum size from a partition
#'
#' @param partition - graph partition, a named vector.
#'
#' @param min_size - size cut off.
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
#' filter_modules(partition, min_size = 5)
filter_modules <- function(partition, min_size = 3) {
  # Remove modules smaller than minimum size from a partition.
  partition[partition %in% as.numeric(names(table(partition))[table(partition) < min_size])] <- 0
  return(partition)
}
