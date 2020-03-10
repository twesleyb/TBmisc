#' normalize_SL
#'
#' Performs sample loading normalization based on column sums of TMT-runs.
#' This function also removes rows in which qc was not quantified in all three replicates.
#'
#' @param data_in - expression data.
#' 
#' @param colID - identifier for data columns.
#'
#' @param groups - vector of exerpimental groups.
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
#' normalize_SL(data_in, colID = "", groups = "")
normalize_SL <- function(data_in, colID = "", groups = "") {
  # Coerce to data.frame.
  data_in <- as.data.frame(data_in)
  # Seperate data columns and info columns
  tmt_cols <- grep(colID, colnames(data_in))
  info_cols <- data_in[, c(1:ncol(data_in))[-tmt_cols]]
  data_cols <- data_in[, tmt_cols]
  # Insure 0 is NA
  data_cols[data_cols == 0] <- NA
  # Loop through groups, calculate SL norm
  for (i in 1:length(groups)) {
    group_cols <- grep(groups[i], colnames(data_cols))
    sub_data <- data_cols[, group_cols]
    target <- mean(colSums(sub_data, na.rm = TRUE), na.rm = TRUE)
    norm_facs <- target / colSums(sub_data, na.rm = TRUE)
    sl_data <- sweep(sub_data, 2, norm_facs, FUN = "*")
    data_cols[, group_cols] <- sl_data
  }
  # Bind info columns and data columns
  data_out <- cbind(info_cols, data_cols)
  return(data_out)
}
