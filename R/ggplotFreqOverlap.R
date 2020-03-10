#' ggplotFreqOverlap
#'
#' Function for plotting overlap frequency for peptides and proteins:
#'
#' @param data_in - input data
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
#' ggplotFreqOverlap(data_in, colID, groups)
ggplotFreqOverlap <- function(data_in, colID, groups) {
  # Subset the data.
  data_in <- as.data.frame(data_in)
  cols <- grepl(colID, colnames(data_in))
  data_work <- data_in[, cols]
  rownames(data_work) <- paste(data_in$Accession,
    c(1:nrow(data_in)),
    sep = "_"
  )
  # Logical matrix. 1 if expressed. 0 if NA, missing.
  data_logic <- !is.na(data_work)
  # Determine if protein was expressed in an experiment (all=TRUE)
  cols_list <- lapply(as.list(groups), function(x) grepl(x, colnames(data_logic)))
  logic <- lapply(cols_list, function(x) apply(data_logic[, x], 1, function(y) all(y)))
  all_expressed <- as.data.frame(do.call(cbind, logic))
  colnames(all_expressed) <- groups
  # Sum rows. The number of experiments a protein/peptide was expressed in.
  all_expressed$Frequency <- rowSums(all_expressed)
  # Summarize with table, and convert to df.
  df <- as.data.frame(table(all_expressed$Frequency))
  # Ignore row with 0, not expressed in any experiment.
  df <- df[!df$Var1 == 0, ]
  # Calculate percentage of total.
  df$Percent <- paste(round(100 * (df$Freq / sum(df$Freq)), 1), "%", sep = "")
  df$ypos <- 0.1 * max(df$Freq)
  # Create plot.
  plot <- ggplot(df, aes(x = Var1, y = Freq, fill = Var1)) +
    geom_col() +
    scale_fill_grey(start = 0.8, end = 0.2) +
    labs(
      title = "Identification Overlap",
      x = "Experiment Overlap",
      y = "Frequency"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, color = "black", size = 11, face = "bold"),
      axis.title.x = element_text(color = "black", size = 11, face = "bold"),
      axis.title.y = element_text(color = "black", size = 11, face = "bold"),
      legend.position = "none"
    )
  # Add percent of total annotation.
  plot <- plot + annotate("text",
    x = df$Var1, y = df$ypos,
    label = df$Percent, size = 3, color = "black"
  )
  return(plot)
}
