#' filter_QC
#'
#' This function bins peptides by mean intensity of QC samples, calculates
#' the log ratio of all QC sample comparisons and then removes a peptides
#' if its mean QC ratio is outside NxSD the mean its intensity bin.
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
#' filter_QC(data_in, groups, nbins, threshold)
filter_QC <- function(data_in, groups, nbins, threshold) {
  for (i in 1:length(groups)) {
    group <- groups[i]
    # Get the data for the specified group.
    cols <- grep(group, colnames(data_in))
    data_sub <- data_in[, cols]
    rownames(data_sub) <- paste(data_in$Accession, data_in$Sequence, c(1:nrow(data_in)), sep = "_")
    tmt_cols <- grep("Abundance", colnames(data_sub))
    qc_cols <- grep("QC", colnames(data_sub))
    # Remove row if QC was not quantified in all QC replicates.
    out <- is.na(data_sub[, qc_cols])
    logic_out <- matrix(NA, nrow = nrow(data_sub), ncol = 1)
    for (j in 1:nrow(data_sub)) {
      logic_out[j] <- any(out[j, ])
    }
    # Write to data_sub
    data_sub[logic_out, ] <- NA
    # Log2 transform and define tmt and qc columns
    data_work <- log2(data_sub)
    cols_qc <- grep("QC", colnames(data_work))
    cols_tmt <- grep("Abundance", colnames(data_work))
    # Calculate the ratio of QC samples (QC1-QC2, QC1-QC3, QC2-QC3)
    contrasts <- combn(cols_qc, 2)
    ratio_dm <- NULL
    for (m in 1:ncol(contrasts)) {
      idx <- contrasts[1, m]
      idy <- contrasts[2, m]
      ratio_dm <- cbind(ratio_dm, data_work[, idx] - data_work[, idy])
    }
    colnames(ratio_dm) <- paste("ratio_", c(1:ncol(ratio_dm)), sep = "")
    data_work <- cbind(data_work, ratio_dm)
    cols <- grep("ratio_", colnames(data_work))
    if (length(cols) == 1) {
      data_work$ratio_avg <- data_work[, cols]
    } else {
      data_work$ratio_avg <- rowMeans(data_work[, cols], 1, na.rm = TRUE)
    }
    qc_cols <- grep("QC", colnames(data_work))
    data_work$avgQC <- rowMeans(data_work[, qc_cols], 1, na.rm = TRUE)
    data_work$sdQC <- apply(data_work[, qc_cols], 1, FUN = sd, na.rm = TRUE)
    # Calculate bins based on mean intensity of QC replicates.
    rows_ignore <- is.nan(data_work$avgQC)
    data_work$bins[!rows_ignore] <- BurStMisc::ntile(data_work$avgQC[!rows_ignore], nbins,
      na.rm = TRUE,
      checkBleed = FALSE, result = "numeric"
    )
    sdQC <- aggregate(data_work$ratio_avg, by = list(bin = data_work$bins), FUN = sd)
    avgQC <- aggregate(data_work$ratio_avg, by = list(bin = data_work$bins), FUN = mean)
    # Loop through bins and determine if mean ratio is outside N*SD away from mean precision.
    logic <- matrix(NA, nrow = nrow(data_work), ncol = 1)
    for (k in 1:nrow(data_work)) {
      kbin <- data_work$bins[k]
      kmean <- avgQC$x[kbin]
      ksd <- sdQC$x[kbin]
      logic[k] <- data_work$ratio_avg[k] > kmean + threshold * ksd || data_work$ratio_avg[k] < kmean - 1 * threshold * ksd
    }
    # Print number of peptides that will be removed:
    num_filt <- length(logic[logic == TRUE]) - length(logic[is.na(logic)])
    message(paste(num_filt, "peptides will be removed from ", group, " because of QC imprecision..."))
    # Insure NA in logical vector are TRUE
    logic[is.na(logic)] <- TRUE
    # Add logic column.
    data_work$out <- logic
    # write to data_filt (unlog)
    data_filt <- 2.^data_work[, tmt_cols]
    data_filt[logic, ] <- NA
    # write to data_in
    cols <- grep(group, colnames(data_in))
    data_in[, cols] <- data_filt
  }
  return(data_in)
}
