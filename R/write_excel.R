#' Write data to excel workbook.
#'
#' @param data - list of data frames to be written to an excel document.
#'
#' @return none
#'
#' @author Tyler W Bradshaw, \email{tyler.w.bradshaw@duke.edu}
#'
#' @references none
#'
#' @keywords write excel write.excel documnent xlsx xls
#'
#' @examples
#' write.excel(data, file = "foo.xlsx")
#' @export

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
