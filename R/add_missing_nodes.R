#' add_missing_nodes
#'
#' add missing nodes to a partition
#'
#' @param p1 - partition 1 - named vector
#'
#' @param p2 - partition 2 - named vector
#'
#' @return none
#'
#' @author Tyler W Bradshaw, \email{tyler.w.bradshaw@duke.edu}
#'
#' @references none
#'
#' @keywords none
#'
#' @import dplyr
#'
#' @export
#'
#' @examples
#' add_missing_nodes(p1, p2)
add_missing_nodes <- function(p1, p2, ...) {
  # Add missing nodes to a partition.
  parts_list <- c(as.list(environment()), list(...))
  all_nodes <- unique(unlist(sapply(parts_list, function(x) names(x))))
  new_parts <- sapply(parts_list, function(x) {
    missing <- all_nodes[all_nodes %notin% names(x)]
    new_nodes <- vector(mode = "numeric", length(missing))
    names(new_nodes) <- missing
    new_part <- c(new_nodes, x)
    out <- duplicated(names(new_part))
    return(list(new_part[!out]))
  })
  names(new_parts) <- paste0("p", seq(length(new_parts)))
  return(new_parts)
}
