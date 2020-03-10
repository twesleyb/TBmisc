optimizeMCL <- function(prots,adjm, inflation = seq(1.2,5,by=0.1),quiet=FALSE){
	# Function to perform MCL at several inflation values.
	# Uses network enhancement to denoise graph--otherwise MCL won't work.
	# Returns partition that maximizes modularity of initial graph.
	# Get enhanced subgraph.
	g <- graph_from_adjacency_matrix(adjm, mode="undirected",weighted=TRUE)
	subg <- induced_subgraph(g,vids = V(g)[prots])
	subg_ne <- neten(subg)
	subg <- set_edge_attr(subg,name="weight",index=which(E(subg)$weight < 0),value=0)
	# Explore inflation space.
	q <- vector("numeric",length(inflation))
	parts <- list()
	if (!quiet) {
		message("Exploring inflation space in order to find best MCL partition...")
		pbar <- txtProgressBar(min=0,max=length(q),style=3)
	}
	for (i in seq_along(inflation)) {
		part <- clusterMCL(subg_ne,weight="weight",inflation=inflation[i])
		# Modularity based on initial graph.
		q[i] <- modularity(subg,
				   membership=part[V(subg)],weights=E(subg)$weight)
		parts[[i]] <- part
		if (! quiet) { setTxtProgressBar(pbar,i) }
	}
	if (! quiet) { close(pbar); message("\n") }
	# Get best partition.
	id_best <- which(q==max(q))
	best_i <- inflation[id_best]
	best_q <- q[id_best]
	if (!quiet) {
		message(paste("... .Best inflation:",best_i))
		message(paste("... Best modularity:",best_q))
	}
	# Return best partition.
	best_p <- parts[[id_best]]
	return(best_p)
}
