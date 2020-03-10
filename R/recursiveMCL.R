recursiveMCL <- function(initial_partition,adjm,max_size=100) {
	# Intial partition is named module membership vector.
	# adjm - is adjacency matrix describing the graph.
	# max_size - is the maximum allowable size of a module.
	# Calls optimizeMCL() to generate optimized mcl partitions.
	cluster_list <- split(initial_partition,initial_partition)
	too_big <- sapply(cluster_list,length) > max_size
	message(paste("Recursively splitting",
		      sum(too_big),"module(s) by MCL..."))
	while(any(too_big)) {
		# Keep looping while there are any modules that are too big.
		idx <- which(too_big)[1]
		prots <- names(cluster_list[[idx]])
		partition <- optimizeMCL(prots,adjm)
		modules <- split(partition,partition)
		modules[[idx]] <- NULL
		cluster_list <- c(cluster_list,modules)
		too_big <- sapply(cluster_list,length) > max_size
	}
	# Collect modules in new partition vector.
	names(cluster_list) <- c(1:length(cluster_list))
	partition <- unlist(cluster_list)
	names(partition) <- sapply(strsplit(names(partition),"\\."),"[",2)
	return(partition)
}
