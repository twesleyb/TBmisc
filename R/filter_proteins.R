#' filter_proteins
#'
#' remove one hit wonders and proteins that are not identified in at least
#' 50% of samples.
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
#' filter_proteins(data_in, colID)
filter_proteins <- function(data_in, colID) {
  data_in <- as.data.frame(data_in)
  # Removing one hit wonders...
  out <- data_in$Peptides == 1
  message(paste(
    length(out[out == TRUE]),
    "proteins are identified by only one peptide and will be removed."
  ))
  filt_protein <- data_in[!out, ]
  # Removing proteins that are not identified in at least 50% of samples.
  tmt_cols <- grep(colID, colnames(filt_protein))
  threshold <- length(colnames(filt_protein)[tmt_cols]) / 2
  out <- apply(filt_protein[, tmt_cols], 1, function(x) sum(is.na(x))) > threshold
  # Number of proteins identified in less than 50% of samples
  message(paste(
    length(out[out == TRUE]),
    "proteins are identified in less than 50% of samples and are removed."
  ))
  filt_protein <- filt_protein[!out, ]
  return(filt_protein)
}
