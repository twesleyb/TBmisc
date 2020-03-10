#' normalize_IRS
#'
#' Performs IRS normalization
#' Supports geometric or robust mean.
#'
#' @param data_in - expression data
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
#' normalize_IRS(data_in, IRS_ID = "QC", groups)
normalize_IRS <- function(data_in, IRS_ID, groups, robust = TRUE) {
  # Subset the data.
  cols_qc <- grep(IRS_ID, colnames(data_in))
  data_qc <- data_in[, cols_qc]
  # Empty list for output of loop.
  data_list <- list()
  # Loop to calculate rowMeans of QC channels within an experiment.
  for (i in 1:length(groups)) {
    exp_cols <- grep(groups[i], colnames(data_qc))
    df <- as.data.frame(rowMeans(data_qc[, exp_cols], na.rm = TRUE, dims = 1))
    colnames(df) <- paste(groups[1], "QC", "rowMeans", sep = "_")
    data_list[[i]] <- df
  }
  # Bind the data frames in list.
  df_IRS <- do.call(cbind, data_list)
  # Calculate mean of QC means (supports Robust, geometric mean).
  if (robust == TRUE) {
    # Calculate geometric rowMeans, ignore missing values.
    df_IRS$Avg <- apply(df_IRS, 1, function(x) exp(mean(log(x), na.rm = TRUE)))
    message("Used robust (geometric) mean.")
  } else {
    df_IRS$Avg <- apply(df_IRS, 1, function(x) mean(x, na.rm = TRUE))
    message("Used arithmetic mean.")
  }
  # Compute scaling factors for each experiment.
  factors_list <- list()
  for (j in 1:length(groups)) {
    factors_list[[j]] <- df_IRS$Avg / df_IRS[, j]
  }
  # Loop through factors list and generate matrix of factors, store in a new list.
  new_list <- list()
  for (k in 1:length(groups)) {
    num_channels <- length(grep(groups[k], colnames(data_in)))
    new_list[[k]] <- matrix(factors_list[[k]], nrow = nrow(data_in), ncol = num_channels)
  }
  # Bind factors in list.
  dm_factors <- do.call(cbind, new_list)
  # Perform IRS (factors * data_in)
  tmt_cols <- grep("Abundance", colnames(data_in))
  data_IRS <- data_in
  data_IRS[, tmt_cols] <- dm_factors * data_in[, tmt_cols]
  # Return IRS data.
  return(data_IRS)
}
