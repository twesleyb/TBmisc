# Function to write graph edge list to file in a-b-c format.
write_abc <- function(graph, filename, weights = NULL) {
  # Writes igraph object edge list to file.
  # graph - igraph object.
  # filename - output filename, passed to data.table::fwrite.
  # weights - optional character specifying igraph edge weights.
  require(data.table)
  df <- data.table(as_edgelist(graph, names = TRUE))
  colnames(df) <- c("nodeA", "nodeB")
  if (!is.null(weights)) {
    df$weight <- E(g)[weights]
  }
  fwrite(df, file = filename, sep = " ", col.names = FALSE, row.names = FALSE)
}
