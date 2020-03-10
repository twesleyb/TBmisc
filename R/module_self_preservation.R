# Function to perform module self preservation:
module_self_preservation <- function(partitions,resolution){
  # Get partition.
  partition <- as.integer(partitions[resolution, ]) 
  # Add 1 so that all module assignments >0.
  if (min(partition)==0) { partition <- partition + 1 }
  partition <- reset_index(partition)
  names(partition) <- colnames(partitions)
  module_list <- list(self = partition)
  # Perform permutation test for module self-preservation.
  suppressWarnings({
    selfPreservation <- NetRep::modulePreservation(
      network = network_list,
      data = data_list,
      correlation = correlation_list,
      moduleAssignments = module_list,
      modules = NULL,
      backgroundLabel = 0,
      discovery = "self",
      test = "self",
      selfPreservation = TRUE,
      nThreads = nThreads,
      nPerm = NULL, # Determined automatically by the function.
      null = "overlap",
      alternative = "greater", # Greater for self-preservation.
      simplify = TRUE,
      verbose = verbose
    )
  })
  # Remove NS modules--set NS modules to 0.
  preservedParts <- check_modules(selfPreservation, strength, stats)
  nModules <- length(unique(partition))
  out <- names(preservedParts)[preservedParts == "ns"]
  partition[partition %in% out] <- 0
  nPreserved <- nModules - length(out)
  return(partition)
}
