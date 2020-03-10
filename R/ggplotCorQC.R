#' ggplotCorQC
#'
#' Scatter plot showing correlation of QC samples. All comparisons.
#' nbin number of histograms showind distribution of QC ratios for all intensity bins.
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
#' @import grid gridExtra ggplot2
#'
#' @export
#'
#' @examples
#' ggplotCorQC(data_in, groups, colID, nbins, annotate = TRUE)
ggplotCorQC <- function(data_in, groups, colID, nbins, annotate = TRUE) {
	suppressPackageStartupMessages({
		library(ggplot2)
		library(gtable)
		library(grid)
		library(gridExtra)
	})
  plot_list <- list()
  for (i in 1:length(groups)) {
    cols <- grep(groups[i], colnames(data_in))
    data_sub <- data_in[, cols]
    QCcols <- grep(colID, colnames(data_sub))
    data_work <- na.omit(data_sub[, QCcols])
    contrasts <- combn(c(1:ncol(data_work)), 2) # QC comparisons.
    num_iter <- dim(contrasts)[2]
    data_list <- list()
    for (k in 1:num_iter) {
      x <- contrasts[1, k]
      y <- contrasts[2, k]
      Log2QC1 <- log2(data_work[, x])
      Log2QC2 <- log2(data_work[, y])
      Ratio <- Log2QC1 - Log2QC2
      data_list[[k]] <- cbind(Log2QC1, Log2QC2, Ratio)
    }
    # merge data frames in list.
    data <- as.data.frame(do.call(rbind, data_list))
    # Bin by mean intensity.
    mu <- rowMeans(data_work)
    data$bins <- rep(BurStMisc::ntile(mu, nbins, na.rm = TRUE, 
				      checkBleed = FALSE, 
				      result = "numeric"), num_iter)
    # Determine best fit line.
    fit <- lm(data$Log2QC1 ~ data$Log2QC2)
    # Calculate Pearson P-Value.
    corTest <- cor.test(~ data$Log2QC1 + data$Log2QC2,
      data = cbind(data$Log2QC1, data$Log2QC2),
      method = "pearson", conf.level = 0.95
    )
    Slope <- paste("Slope =", round(coef(fit)[2], 4))
    R2 <- paste("R2 =", round(corTest$estimate, 4))
    # Generate scatter plot.
    plot <- ggplot(data, aes(x = Log2QC1, y = Log2QC2, color = bins)) + geom_point() +
      scale_color_continuous(name = "Intensity Bin") +
      # geom_abline(intercept=coef(fit)[1],slope=coef(fit)[2], color = "black", linetype = "dashed") +
      ggtitle(groups[i]) +
      xlab(expression(Log[2] ~ QC1)) +
      ylab(expression(Log[2] ~ QC2)) +
      theme(
        plot.title = element_text(hjust = 0.5, color = "black", size = 11, face = "bold"),
        axis.title.x = element_text(color = "black", size = 11, face = "bold"),
        axis.title.y = element_text(color = "black", size = 11, face = "bold")
      )
    # Add annotation layer.
    mytable <- rbind(R2, Slope)
    xrange <- unlist(ggplot_build(plot)$layout$panel_params[[1]]$x.range)
    yrange <- unlist(ggplot_build(plot)$layout$panel_params[[1]]$y.range)
    xmin <- min(xrange)
    xmax <- max(xrange)
    xdelta <- xmax - xmin
    ymin <- min(yrange)
    ymax <- max(yrange)
    ydelta <- ymax - ymin
    tt <- ttheme_default(base_size = 11, 
			 core = list(bg_params = list(fill = "white")))
    tab <- tableGrob(mytable, rows = NULL, theme = tt)
    g <- gtable_add_grob(tab,
      grobs = rectGrob(gp = gpar(fill = NA, lwd = 1)),
      t = 1, b = nrow(tab), l = 1, r = ncol(tab)
    )
    if (annotate == TRUE) {
      plot <- plot + annotation_custom(g,
        xmin = xmin - 0.65 * xdelta, xmax,
        ymin = ymin + 0.8 * ydelta, ymax
      )
    }
    plot_list[[i]] <- plot
    names(plot_list)[[i]] <- groups[[i]]
  }
  return(plot_list)
}
