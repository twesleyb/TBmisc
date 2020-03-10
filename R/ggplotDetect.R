#' ggplotDetect
#'
#' Function for plotting distribution (density) of missing values).
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
#' @import ggplot2
#'
#' @export
#'
#' @examples
#' ggplotDetect(data_in, group, log = TRUE)
ggplotDetect <- function(data_in, group, log = TRUE) {
  # Subset the data.
  cols <- grepl(group, colnames(data_in))
  data_work <- as.data.frame(data_in[, cols])
  # Remove rows will all missing values.
  data_work$NA_count <- apply(data_work, 1, function(x) sum(is.na(x)))
  data_work <- subset(data_work, !NA_count == (ncol(data_work) - 1))
  # Remove rows with missing QC data.
  # cols <- grep("QC",colnames(data_work))
  # data_work$NA_count <- apply(data_work[,cols],1,function(x) sum(is.na(x)))
  # data_work <- subset(data_work,!NA_count==length(cols))
  # Number of remaining missing values.
  data_work$NA_count <- apply(data_work[, c(1:ncol(data_work) - 1)], 1, function(x) sum(is.na(x)))
  # sum(data_work$NA_count)
  # Add Row avg.
  data_work$RowAverage <- apply(data_work[, c(1:ncol(data_work) - 1)], 1, function(x) mean(x, na.rm = TRUE))
  # Get the data for plotting.
  if (log == TRUE) {
    df <- as.data.frame(cbind(Avg = log2(data_work$RowAverage), Group = data_work$NA_count))
  } else {
    df <- as.data.frame(cbind(Avg = data_work$RowAverage, Group = data_work$NA_count))
  }
  # Groups are 1 (TRUE; No missing values), and 0 (FALSE; contains missing values)
  df$Group <- !df$Group == 0
  # df$Group[df$Group==1] <- "Complete"
  # df$Group[df$Group==0] <- "Missing"
  df$Group <- as.factor(df$Group)
  plot <- ggplot(df, aes(x = Avg, fill = Group, colour = Group)) +
    geom_density(alpha = 0.1, size = 1) + ggtitle(paste(group, "missing values")) +
    scale_fill_discrete(name = "Missing Values") +
    scale_color_discrete(name = "Missing Values") +
    xlab(expression(Log[2] ~ Intensity)) +
    ylab(expression(Density)) +
    theme(
      plot.title = element_text(hjust = 0.5, color = "black", size = 11, face = "bold"),
      axis.title.x = element_text(color = "black", size = 11, face = "bold"),
      axis.title.y = element_text(color = "black", size = 11, face = "bold")
    )
  return(plot)
}
