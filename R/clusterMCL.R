clusterMCL <- function(graph, weight = NULL, inflation = 2.5) {
  # Cluster an igraph object with the MCL algorithm.
  # Returns partition of the graph.
  suppressPackageStartupMessages({
    require(data.table)
    require(igraph)
  })
  df <- data.table(as_edgelist(graph, names = TRUE))
  if (!is.null(weight)) {
    df$weight <- edge_attr(graph, weight, index = E(graph))
  }
  infile <- paste0(as.numeric(Sys.time()),"_tempnet.csv")
  fwrite(df, file = infile, sep = " ", col.names = FALSE, row.names = FALSE)
  intype <- paste0("--", "abc")
  inflation <- paste("-I", inflation)
  # mcl yeast_network.csv --abc -I 1 -o yeast_clusters.txt
  cmd <- paste("mcl", infile, intype, inflation, "-o -")
  mcl_result <- system(cmd,
    intern = TRUE,
    ignore.stdout = FALSE, ignore.stderr = TRUE
  )
  # Parse the result.
  unlink(infile)
  modules <- strsplit(mcl_result, "\t")
  module_size <- sapply(modules, length)
  # Generate named partition vector.
  partition <- rep(c(1:length(modules)), times = module_size)
  names(partition) <- unlist(modules)
  return(partition)
}
