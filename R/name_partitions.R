name_partitions <- function(partitions,
			    rprefix="R",mprefix="M",output="modules") {
	# Give partitions and modules a prefix.
	 names(partitions) <- paste0(rprefix,c(1:length(partitions)))
	  parts <- lapply(partitions,function(x) split(x,x))
	  parts <- lapply(parts,function(x) { 
				  names(x) <- paste0(mprefix,names(x))
				  return(x) })
	  if (output == "modules") {
	    return(unlist(parts,recursive=FALSE))
	  } else if (output == "partition") {
	    return(parts)
	  }
	}
