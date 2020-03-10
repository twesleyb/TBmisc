createCytoscapeModuleGraph <- function(partition,ME_list,title="Network") {
	suppressPackageStartupMessages({ 
		library(RCy3) 
	        library(neten)
	})
	cytoscape_ping()
	# Collect modules.
	modules <- split(partition,partition)
        names(modules) <- paste0("M",names(modules))
	# Remove M0.
	modules <- modules[-which(names(modules) =="M0")]
	# Module sizes.
        module_size <- sapply(modules,length)
	# Calculate correlation between module eigengenes.
	adjm_me <- cor(do.call(cbind,ME_list))
	# Replace negative weights with 0.
	adjm_me[adjm_me<0] <- 0
        # Perform network enhancement.
	adjm_ne <- neten(adjm_me)
	# Build a graph.
	g0 <- graph_from_adjacency_matrix(adjm_me,mode="undirected",
				   weighted=TRUE,diag=FALSE)
	g <- graph_from_adjacency_matrix(adjm_ne,mode="undirected",
				   weighted=TRUE,diag=FALSE)
	g <- set_vertex_attr(g,"color",value = module_colors[names(V(g))])
	g <- set_vertex_attr(g,"size", value = module_size[names(V(g))]) 
	# Identify edge weight cutoff at which to threshold the graph.
	nEdges <- length(E(g))
	e_max <- max(abs(E(g)$weight))
	e_min <- min(abs(E(g)$weight))
	cutoffs <- seq(e_min,e_max,by=0.01)
	check <- vector("logical",length=length(cutoffs))
	for (k in seq_along(cutoffs)){
		g_temp <- g
		idx <- which(abs(E(g_temp)$weight) <= cutoffs[k])
		g_temp <- delete.edges(g_temp,idx)
		check[k] <- is.connected(g_temp)
	}
	# Prune weak edges.
	threshold <- cutoffs[max(which(check))]
	idx <- which(abs(E(g)$weight) <= threshold)
	g <- delete.edges(g,idx)
	# Add original weights as edge attribute.
	g <- set_edge_attr(g, "cor", value=E(g0)[E(g)]$weight)
	# Send to Cytoscape. 
	net <- createNetworkFromIgraph(g, title)
	# Create a visual style.
	  style.name = "myStyle"
	  # DEFAULTS:
	  defaults = list(
	    NODE_SHAPE = "ellipse",
	    EDGE_TRANSPARENCY = 155,
	    NODE_LABEL_TRANSPARENCY = 255,
	    NODE_LABEL_FONT_SIZE = 12,
	    NODE_LABEL_COLOR = col2hex("black"),
	    NODE_BORDER_TRANSPARENCY = 200,
	    NODE_BORDER_WIDTH = 2,
	    NODE_BORDER_PAINT = col2hex("black"),
	    NODE_TRANSPARENCY = 200,
	    NETWORK_BACKGROUND_PAINT = col2hex("white")
	  )
	  # MAPPED PROPERTIES:
	    mappings <- list(
	      NODE_LABELS = mapVisualProperty('node label','id','p'),
	      NODE_FILL_COLOR = mapVisualProperty('node fill color','color','p'),
	      NODE_SIZE = mapVisualProperty('node size','size','c', 
					    c(min(V(g)$size),max(V(g)$size)), 
					    c(35,100)),
	      EDGE_STROKE_UNSELECTED_PAINT = mapVisualProperty('edge stroke unselected paint','cor','c',
								   c(min(E(g)$cor),max(E(g)$cor)),
								   c(col2hex("light gray"),col2hex("dark red")))
	    )
	  # Create a visual style.
	  createVisualStyle(style.name, defaults = defaults, mappings = mappings)
	  # Apply to graph.
	  setVisualStyle(style.name)
	  # Wait a couple of seconds...
	  Sys.sleep(2)
	  # Save image.
	  fitContent()
	  Sys.sleep(2) # Wait... 
	  cytoscapeFreeMemory()
	  return(net)
	} #Ends function.
