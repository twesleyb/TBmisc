#' peptide_overlap
#'
#' Calculates peptide identification overlap for given contrasts matrix.
#'
#' @param data_in - input data
#'
#' @param contrasts - contrasts
#'
#' @param info_cols - cols to be ignored.
#'
#' @return none
#'
#' @author Tyler W Bradshaw, \email{tyler.w.bradshaw@duke.edu}
#' @references \url{}
#' @keywords
#'
#' @export
#'
#' @examples
#' peptide_overlap(data_in, contrasts, info_cols)
peptide_overlap <- function(data_in, contrasts, info_cols) {
  data_in <- as.data.frame(data_in)
  num_iter <- dim(contrasts)[2]
  contrasts <- rbind(contrasts, matrix(NA, nrow = 3, ncol = num_iter))
  for (i in 1:num_iter) {
    IDa <- contrasts[1, i]
    tmt_cols <- grep(IDa, colnames(data_in))
    data_sub <- na.omit(data_in[, c(info_cols, tmt_cols)])
    listA <- unique(data_sub$Sequence)
    IDb <- contrasts[2, i]
    tmt_cols <- grep(IDb, colnames(data_in))
    data_sub <- na.omit(data_in[, c(info_cols, tmt_cols)])
    listB <- unique(data_sub$Sequence)
    overlap <- length(unique(intersect(listA, listB)))
    total <- length(unique(union(listA, listB)))
    percent <- 100 * (overlap / total)
    contrasts[3, i] <- overlap
    contrasts[4, i] <- total
    contrasts[5, i] <- round(percent, 4)
  }
  return(contrasts)
}
