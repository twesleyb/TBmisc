#' ggplotScaleFreeFit
#'
#' evaluate the scale free fit of a graph
#'
#' @param
#'
#' @return none
#'
#' @author Tyler W Bradshaw, \email{tyler.w.bradshaw@duke.edu}
#'
#' @references none
#'
#' @keywords none
#'
#' @import
#'
#' @export
#'
#' @examples
#' ggplotScaleFreePlot(connectivity)
ggplotScaleFreeFit <- function(connectivity, nBreaks = 10, truncated = FALSE,
                               removeFirst = FALSE, main = "", ...) {
  suppressPackageStartupMessages({
    require(WGCNA)
    require(normalp)
    require(ggplot2)
  })
  k <- connectivity
  discretized.k <- cut(k, nBreaks)
  dk <- tapply(k, discretized.k, mean)
  p.dk <- as.vector(tapply(k, discretized.k, length) / length(k))
  breaks1 <- seq(from = min(k), to = max(k), length = nBreaks + 1)
  hist1 <- suppressWarnings(hist(k,
    breaks = breaks1, equidist = FALSE,
    plot = FALSE, right = TRUE
  )) # ...
  dk2 <- hist1$mids
  dk <- ifelse(is.na(dk), dk2, dk)
  dk <- ifelse(dk == 0, dk2, dk)
  p.dk <- ifelse(is.na(p.dk), 0, p.dk)
  log.dk <- as.vector(log10(dk))
  if (removeFirst) {
    p.dk <- p.dk[-1]
    log.dk <- log.dk[-1]
  }
  log.p.dk <- as.numeric(log10(p.dk + 1e-09))
  lm1 <- lm(log.p.dk ~ log.dk)
  pvalue <- lmp(lm1)
  title <- paste0(
    main, " Scale Free R2 =", as.character(round(summary(lm1)$adj.r.squared, 2)),
    ", slope =", round(lm1$coefficients[[2]], 2)
  )
  OUTPUT <- data.frame(
    scaleFreeRsquared = round(summary(lm1)$adj.r.squared, 2),
    slope = round(lm1$coefficients[[2]], 2)
  )
  # Generate ggplot.
  df <- as.data.frame(cbind(log.dk, log.p.dk))
  plot <- ggplot(df, aes(x = log.dk, y = log.p.dk)) + geom_point(size = 2) +
    ggtitle(title) +
    geom_abline(intercept = coef(lm1)[1], slope = coef(lm1)[2], color = "black", linetype = "dashed") +
    labs(y = expression(Log[10](p(k)))) +
    labs(x = expression(Log[10](k))) +
    theme(
      plot.title = element_text(hjust = 0.5, color = "black", size = 11, face = "bold"),
      axis.title.x = element_text(hjust = 0.5, color = "black", size = 11),
      axis.title.y = element_text(hjust = 0.5, color = "black", size = 11)
    )
  return(plot)
}
