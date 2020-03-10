#' ggplotQCHist
#'
#' plot histogram of qc ratios
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
#' @import ggplot2
#'
#' @export
#'
#' @examples
#' ggplotQCHist(data_in, groups, nbins, threshold)
ggplotQCHist <- function(data_in, group, nbins, threshold) {
  hist_list <- list()
  # Get the data for the specified group.
  cols <- grep(group, colnames(data_in))
  data_sub <- data_in[, cols]
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
  data_work$ratio_1 <- data_work[, 1] - data_work[, 2]
  data_work$ratio_2 <- data_work[, 1] - data_work[, 3]
  data_work$ratio_3 <- data_work[, 2] - data_work[, 3]
  cols <- grep("ratio_", colnames(data_work))
  cols_ratio <- (last(cols_tmt) + 1):(last(cols_tmt) + 3)
  data_work$ratio_avg <- rowMeans(data_work[, cols_ratio], 1, na.rm = TRUE)
  data_work$avgQC <- rowMeans(data_work[, cols_qc], 1, na.rm = TRUE)
  data_work$sdQC <- apply(data_work[, cols_ratio], 1, FUN = sd, na.rm = TRUE)
  # Calculate bins based on mean intensity of QC replicates.
  rows_ignore <- is.nan(data_work$avgQC)
  data_work$bins[!rows_ignore] <- BurStMisc::ntile(data_work$avgQC[!rows_ignore], nbins,
    na.rm = TRUE,
    checkBleed = FALSE, result = "numeric"
  )
  # Calculate summary statistics of Intensity bins
  data_temp <- data_work[!rows_ignore, ]
  data_QC <- subset(data_temp) %>%
    group_by(bins) %>%
    dplyr::summarize(
      min = min(ratio_avg, na.rm = TRUE),
      max = max(ratio_avg, na.rm = TRUE),
      avg = mean(ratio_avg, na.rm = TRUE),
      stdev = sd(ratio_avg, na.rm = TRUE)
    )
  # Add bin avg and std to data_temp.
  data_temp$bin_avg <- NA
  data_temp$bin_std <- NA
  for (i in 1:nbins) {
    data_temp$bin_avg[data_temp$bins == i] <- data_QC$avg[data_QC$bins == i]
    data_temp$bin_std[data_temp$bins == i] <- data_QC$stdev[data_QC$bins == i]
  }
  # Add upper and lower bounds.
  data_temp$upper <- 4 * data_temp$bin_std + data_temp$bin_avg
  data_temp$lower <- -4 * data_temp$bin_std + data_temp$bin_avg
  # Out if ratio is outside lower/upper bounds.
  data_temp$out1 <- data_temp$ratio_avg > data_temp$upper
  data_temp$out2 <- data_temp$ratio_avg < data_temp$lower
  # Check if value is outside.
  data_temp$out <- as.numeric(apply(data_temp, 1, function(x) any(x[c(23, 24)])))
  # Count number of peptides to be removed.
  data_QC2 <- subset(data_temp) %>%
    group_by(bins) %>%
    dplyr::summarize(
      min = min(ratio_avg, na.rm = TRUE),
      max = max(ratio_avg, na.rm = TRUE),
      avg = mean(ratio_avg, na.rm = TRUE),
      stdev = sd(ratio_avg, na.rm = TRUE),
      nout = sum(out, na.rm = TRUE)
    )
  # Loop through bins and generate plots.
  palette <- c("#132B43", "#22496C", "#336A98", "#448DC6", "#56B1F7")
  for (j in 1:nbins) {
    data_sub <- subset(data_work, bins == j)
    idy <- grep("ratio_avg", colnames(data_sub))
    dm <- as.matrix(data_sub[, idy])
    data_temp <- na.omit(melt(dm))
    mu <- mean(data_temp$value)
    stdev <- sd(data_temp$value)
    plot <- ggplot(data = data_temp, aes(value)) + geom_histogram(bins = 100, fill = palette[j]) +
      ggtitle(paste("Intensity bin =", j)) +
      geom_vline(xintercept = mu + 4 * stdev, linetype = "dashed", color = "red", size = 0.75) +
      geom_vline(xintercept = mu - 4 * stdev, linetype = "dashed", color = "red", size = 0.75) +
      theme(
        plot.title = element_text(hjust = 0.5, color = "black", size = 11, face = "bold"),
        axis.title.x = element_text(color = "black", size = 11, face = "bold"),
        axis.title.y = element_text(color = "black", size = 11, face = "bold")
      )
    plot <- plot + xlim(c(-1.5, 1.5))
    # Add annotation layer.
    xrange <- unlist(ggplot_build(plot)$layout$panel_params[[1]][1])
    yrange <- unlist(ggplot_build(plot)$layout$panel_params[[1]][8])
    xmin <- min(xrange)
    xmax <- max(xrange)
    xdelta <- xmax - xmin
    ymin <- min(yrange)
    ymax <- max(yrange)
    ydelta <- ymax - ymin
    tt <- ttheme_default(base_size = 11, core = list(bg_params = list(fill = "white")))
    mytable <- t(subset(data_QC2, bins == j))
    mytable <- round(mytable, 2)
    rownames(mytable) <- c("Bin", "Min", "Max", "Mean", "SD", "Nout")
    mytable <- paste(rownames(mytable), mytable, sep = " = ")
    tab <- tableGrob(mytable, rows = NULL, theme = tt)
    g <- gtable_add_grob(tab,
      grobs = rectGrob(gp = gpar(fill = NA, lwd = 1)),
      t = 1, b = nrow(tab), l = 1, r = ncol(tab)
    )
    plot <- plot + annotation_custom(g,
      xmin = xmin + 0.80 * xdelta, xmax,
      ymin = ymin + 0.6 * ydelta, ymax
    )
    hist_list[[j]] <- plot
  }
  return(hist_list)
}
