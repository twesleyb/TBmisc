#' ggplotPeptideBarPlot
#'
#'
#' @param raw_peptide - raw peptide expression data
#'
#' @return none
#'
#' @author Tyler W Bradshaw, \email{tyler.w.bradshaw@duke.edu}
#'
#' @references none
#'
#' @keywords none
#'
#' @import ggplot2
#'
#' @export
#'
#' @examples
#' ggplotPeptideBarPlot(raw_peptide)
ggplotPeptideBarPlot <- function(raw_peptide, seed = 1) {
  # Imports
  suppressPackageStartupMessages({
    require(ggplot2)
    require(reshape2)
  })
  # Get a slice of the data.
  set.seed(seed)
  dat <- as.data.frame(subset(raw_peptide, grepl("Dlg4", raw_peptide$Description)))
  rownames(dat) <- paste(dat$Accession, dat$Sequence, c(1:nrow(dat)), sep = "_")
  idy <- grepl("Shank2", colnames(dat))
  dat <- dat[, idy]
  dat <- na.omit(dat)
  # Make bar plot for given peptide.
  colIDs <- gsub(",", "", sapply(strsplit(colnames(dat), "\\ "), "[", 3))
  geno <- gsub(",", "", sapply(strsplit(colnames(dat), "\\ "), "[", 5))
  n <- sample(nrow(dat), 1)
  df <- suppressMessages(reshape2::melt(dat[n, ]))
  df$Channel <- colIDs
  title <- paste0("Dlg4", " >", strsplit(rownames(dat)[n], "_")[[1]][2])
  plot <- ggplot(df, aes(x = Channel, y = log2(value), fill = Channel)) +
    geom_bar(stat = "identity", width = 0.9, position = position_dodge(width = 1)) +
    xlab("TMT Channel") + ylab("Log2(Raw Intensity)") +
    ggtitle(title) +
    theme(legend.position = "none")
  return(plot)
}
