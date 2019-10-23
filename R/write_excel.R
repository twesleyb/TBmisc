#' write.excel
#'
#' Writes a list of data frames to excel workbook with the openxlsx library.
#'
#' @param data - the named list of dataframes.
#'
#' @return None
#'
#' @author Tyler W Bradshaw, \email{tyler.w.bradshaw@duke.edu}
#'
#' @references
#'
#' @keywords
#'
#' @examples
#' write.excel(data, file = "foobar.xlsx")
#'
#' ## A function to Write to excel.
write.excel <- function(data, file, rowNames = FALSE, colNames = TRUE) {
  require(openxlsx, quietly = TRUE)
  if (class(data) == "list") {
    list <- data
  } else {
    # Coerce to list.
    list <- list(data)
  }
  # Insure there are names.
  if (is.null(names(list))) {
    names(list) <- paste("Sheet", c(1:length(list)))
  }
  wb <- createWorkbook()
  # Loop to add a worksheets:
  for (i in 1:length(list)) {
    df <- as.data.frame(list[[i]])
    addWorksheet(wb, sheetName = names(list[i]))
    writeData(wb, sheet = i, df, rowNames = rowNames, colNames = colNames)
  }
  # Save workbook.
  saveWorkbook(wb, file, overwrite = TRUE)
}
