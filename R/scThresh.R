scThresh  <- function(adjm,
		      from=min(adjm),to=max(adjm),by=0.1,nsteps=NULL) {
	# Attempt to go faster than iterative applicaation of igraph's
	# delete edges and is.connected... Not sure its any faster.
	# Helpful review of graph laplacians:
	# https://towardsdatascience.com/spectral-clustering-aba2640c0d5b
	# Find edge weight at which graph splits into multiple components.
	# Remove edges less than cutoff
	# Diagonal of degree matrix
	# calc laplacian
	# calc eigenvalues
	# if eigenvalue is 0 then unnconnected
	# Check the input arguments.
	# While loop to test cutoffs.
	i <- 1
	is_connected <- TRUE
	if (!is.null(by) & is.null(nsteps)) {
		cutoff <- seq(from,to,by)
	} else if (!is.null(nsteps)) {
		cutoff <- seq(from,to,length.out=nsteps)
	} else {
		stop(paste("Specify either the step size (by)",
			   "or number of steps(nsteps)."))
	}
	while (is_connected){
		if (i > length(cutoff)) { 
			stop("Unable to determine threshold.") 
		}
		A <- adjm
		diag(A) <- 0
		A[A < cutoff[i]] <- 0
		D <- apply(A,2,sum)
		Dm <- diag(D)
		L <- Dm - A
		Ve <- eigen(L,only.values=TRUE)[[1]]
		is_connected <- !any(Ve == 0)
		i <- i + 1 
	}
	threshold <- cutoff[i-1]
	return(threshold)
}
