createCytoscapeCoExpressionGraph <- function(partition,node_colors,
					     DBDcolors,
					     threshold,network_layout,
					     background,title){
	# TODO: speed can be increased by writting to file.
	suppressPackageStartupMessages({ 
		library(RCy3) 
	        library(neten)
	})
	# Check that we are connected to Cytoscape.
	cytoscape_ping()
	# Remove background nodes.
	partition <- partition[partition != background]
	keep <- colnames(adjm) %in% names(partition)
	subAdjm <- adjm[keep,keep]
	# Collect modules.
	modules <- split(partition,partition)
        names(modules) <- paste0("M",names(modules))
        # Perform network enhancement.
	adjm_ne <- neten(subAdjm)
	# Build graphs from adjm and enhanced adjm.
	g0 <- graph_from_adjacency_matrix(adjm,mode="undirected",
				   weighted=TRUE,diag=FALSE)
	g <- graph_from_adjacency_matrix(adjm_ne,mode="undirected",
				   weighted=TRUE,diag=FALSE)
	# Add node color attribute.
	g <- set_vertex_attr(g,"color",
			     value = node_colors[names(V(g))])
	# Add node DBD color attribute.
	g <- set_vertex_attr(g,"DBDcolor",
			     value = DBDcolors[names(V(g))])
	# Prune weak edges.
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
	    NODE_LABEL_TRANSPARENCY = 0,
	    NODE_LABEL_FONT_SIZE = 12,
	    NODE_LABEL_COLOR = col2hex("black"),
	    NODE_BORDER_TRANSPARENCY = 200,
	    NODE_BORDER_WIDTH = 2,
	    NODE_SIZE = 35,
	    NODE_BORDER_PAINT = col2hex("black"),
	    NODE_TRANSPARENCY = 200,
	    NETWORK_BACKGROUND_PAINT = col2hex("white")
	  )
	  # MAPPED PROPERTIES:
	    mappings <- list(
	      NODE_LABELS = mapVisualProperty('node label','id','p'),
	      NODE_FILL_COLOR = mapVisualProperty('node fill color','color','p'),
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
	  layoutNetwork(network_layout)
	  Sys.sleep(2)
	  fitContent()
	  cytoscapeFreeMemory()
	  return(net)
	} #Ends function.
