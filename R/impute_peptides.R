#' impute_peptides
#'
#' Function to impute peptide level missing values.
#' Supports MLE for MAR and KNN for MNAR.
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
#' impute_peptides(data_in, groups, method)
impute_peptides <- function(data_in, groups, method, qc_threshold = 0, bio_threshold = 2) {
	suppressPackageStartupMessages({library(impute)})
	# A function to suppress output from impute.knn.
	quiet <- function(x) {
		sink(tempfile())
		on.exit(sink())
		invisible(force(x))
	}
  n_out <- list()
  for (i in 1:length(groups)) {
    data_work <- data_in
    rownames(data_work) <- paste(data_in$Accession, c(1:nrow(data_in)), sep = "_")
    group <- groups[i]
    cols <- grep(group, colnames(data_work))
    data_sub <- data_work[, cols]
    data_sub$Sort <- c(1:nrow(data_sub))
    # Ignore rows with too many missing values.
    data_sub$qc_NA <- apply(data_sub[, 1:3], 1, function(x) sum(is.na(x)))
    unique(data_sub$qc_NA)
    no_impute_qc <- data_sub$qc_NA > qc_threshold
    data_sub$bio1_NA <- apply(data_sub[, 4:7], 1, function(x) sum(is.na(x)))
    unique(data_sub$bio1_NA)
    data_sub$bio2_NA <- apply(data_sub[, 8:11], 1, function(x) sum(is.na(x)))
    unique(data_sub$bio2_NA)
    no_impute_bio <- data_sub$bio1_NA > bio_threshold | data_sub$bio2 > bio_threshold
    rows_out <- no_impute_bio == TRUE | no_impute_qc == TRUE
    length(rows_out[rows_out == TRUE])
    # Replace values in rows to ignore with NA.
    data_out <- data_sub[rows_out, ]
    data_out[, c(1:11)] <- NA
    # Data for imputing
    data_temp <- data_sub[!rows_out, ]
    data_temp[, c(1:11)] <- log2(data_temp[, c(1:11)])
    # Number of missing values
    num_NA <- is.na(data_temp[, c(1:11)])
    num_NA <- length(num_NA[num_NA == TRUE])
    cat(paste(num_NA, "values from", groups[i], "are missing and will be replaced by imputing.\n", sep = " "))
    n_out[[i]] <- num_NA
    # KNN Impute
    if (method == "knn") {
      data_imp <- as.matrix(data_temp)
      data_imp[, c(1:11)] <- quiet(impute::impute.knn(data_imp[, c(1:11)])$data)
      data_imp <- as.data.frame(data_imp)
    } else if (method == "mle") {
      # MLE Impute
      conditions <- as.factor(c(rep(1, 3), rep(2, 4), rep(3, 4)))
      data_imp <- data_temp
      data_imp[, c(1:11)] <- imp4p::impute.mle(data_temp[, c(1:11)], conditions)
    }
    # Put back together with data_out
    data <- rbind(data_out, 2^data_imp) # Un-log.
    # Sort to original order.
    order <- as.numeric(do.call(rbind, strsplit(rownames(data), "_"))[, 2])
    data$Order <- order
    data <- data[order(data$Order), ]
    # Output
    data_return <- data_sub[, c(1:11)]
    data_return[, c(1:11)] <- data[, c(1:11)]
    data_in[, cols] <- data_return[, c(1:11)]
  }
  names(n_out) <- groups
  results <- list(n_out, data_in)
  names(results) <- c("n_out", "data_imputed")
  return(results)
}
