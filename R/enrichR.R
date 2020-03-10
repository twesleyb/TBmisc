enrichR <- function(gene_list,database,quiet=FALSE){
	# Simple wrapper around enrichR.
	# Supress output from cat within enrichr with capture.output().
	# FIXME: When not run interactively pbar prodcuces a bunchh of 
	# little pbars...
	suppressPackageStartupMessages({
		library(enrichR)
	})
	results <- list()
	n <- length(gene_list)
	if (!quiet) { 
		message(paste("Analyzing",n,"gene groups for",
			      db,"enrichment..."))
		pbar <- txtProgressBar(max=n,style=3) 
	}
	# Loop to perform enrichment analysis.
	for (i in 1:length(gene_list)){
		if (!quiet) { setTxtProgressBar(pbar,i) }
		genes <- gene_list[[i]]
		capture.output({ results[[i]] <- enrichr(genes,db) })
	}
	if (!quiet) { close(pbar) }
	# Collect results.
	results <- unlist(results,recursive=FALSE)
	names(results) <- names(gene_list)
	return(results)
}
