#' summarize_protein
#'
#' Summarize proteins by summing all peptides for a unique Accession identifier.
#'
#' @param data_in - expression data.
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
#' summarize_protein(peptide_data)
summarize_protein <- function(peptide_data,colID="Abundance") {
	suppressPackageStartupMessages({
	library(dplyr)
	library(tibble)
	})
  # Add column for peptides, summarize using dplyr::summarize_all(sum)
  Peptides <- rep(1, nrow(peptide_data))
  temp_data <- tibble::add_column(peptide_data, Peptides, .after = 5)
  tmt_cols <- grep(colID, colnames(temp_data))
  temp_data <- temp_data[, c(2, 6, tmt_cols)]
  prot_data <- temp_data %>%
    group_by(Accession) %>%
    dplyr::summarise_all(funs(sum), na.rm = TRUE)
  # Replace 0 with NA
  prot_data[prot_data == 0] <- NA
  prot_data <- as.data.frame(prot_data)
  return(prot_data)
}
