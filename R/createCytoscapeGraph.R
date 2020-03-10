createCytoscapeGraph <- function(exp_graph,
				 ppi_graph,
				 nodes,
				 module_kme,
				 module_name, 
				 module_colors, 
				 network_layout, 
				 output_file=NULL,
				 image_file=NULL,
				 image_format=NULL){
	## NOTE: Sys.sleep()'s are important to prevent R from getting
        # ahead of Cytoscape!
	suppressPackageStartupMessages({
		library(RCy3)
		library(neten)
	})
	# Check that we are connected to cytoscape.
	cytoscape_ping()
	# Subset graph.
	idx <- match(nodes,names(V(exp_graph)))
	g0 <- induced_subgraph(exp_graph,vids = V(exp_graph)[idx])
	# Add node color attribute.
	g0 <- set_vertex_attr(g0,"color", 
			     value = module_colors[module_name])
	# Add node module attribute.
	g0 <- set_vertex_attr(g0,"module",value = module_name)
	# Add hubiness (KME) attributes.
	g0 <- set_vertex_attr(g0, "kme" ,value=module_kme[names(V(g0))])
	# Network enhancment.
	g <- neten(g0)
	# Prune weak edges.
	nEdges <- length(E(g))
	e_max <- max(E(g)$weight)
	e_min <- min(E(g)$weight)
	cut_off <- seq(e_min,e_max,by=0.01)
	check <- vector("logical",length = length(cut_off))
	# Loop to find threshold.
	for (k in seq_along(cut_off)) {
		threshold <- cut_off[k]
		g_temp <- g
		idx <- which(E(g_temp)$weight <= threshold)
		g_temp <- delete.edges(g_temp,idx)
		check[k] <- is.connected(g_temp)
	}
	cutoff_limit <- cut_off[max(which(check))]
	# Prune edges -- this removes all edge types...
	idx <- which(E(g)$weight <= cutoff_limit)
	g <- delete.edges(g0,E(g0)[E(g)[idx]])
	# Write graph to file this is faster than sending to cytoscape.
	myfile <- file.path(netsdir,paste0(module_name,".gml"))
	write_graph(g,myfile,format="gml")
	# Send to Cytoscape.
	## FIXME: underscores from edge weight attributes are removed!
	winfile <- gsub("/mnt/d/","D:/",myfile)
	cys_net <- importNetworkFromFile(winfile)
	Sys.sleep(2)
	unlink(myfile)
	# Create a visual style.
	style.name <- paste(module_name,"style",sep="-")
	# DEFAULTS:
	defaults = list(
	  NODE_FILL_COLOR = col2hex("gray"),
	  NODE_TRANSPARENCY = 200,
	  NODE_SIZE = 35,
	  NODE_SHAPE = "ellipse",
	  NODE_LABEL_TRANSPARENCY = 255,
	  NODE_LABEL_FONT_SIZE = 12,
	  NODE_LABEL_COLOR = col2hex("black"),
	  NODE_BORDER_TRANSPARENCY = 200,
	  NODE_BORDER_WIDTH = 4,
	  NODE_BORDER_PAINT = col2hex("black"),
	  NODE_TRANSPARENCY = 200,
	  EDGE_STROKE_UNSELECTED_PAINT = col2hex("black"),
	  EDGE_WIDTH = 2,
	  NETWORK_BACKGROUND_PAINT = col2hex("white")
	)
	# MAPPED PROPERTIES:
	mappings <- list(
	  NODE_LABEL = mapVisualProperty('node label', 'symbol', 'p'),
	  NODE_FILL_COLOR = mapVisualProperty('node fill color',
					      'color','p'),
	  NODE_SIZE = mapVisualProperty('node size',
	                                'kme',
	                                'c', 
	                                c(min(V(g)$kme),max(V(g)$kme)), 
	                                c(25,75)),
	  EDGE_TRANSPARENCY = mapVisualProperty('edge transparency',
	                                        'weight', 
	                                        'c', 
	                                        c(min(E(g)$weight),max(E(g)$weight)), 
	                                        c(155,255)),
	  EDGE_STROKE_UNSELECTED_PAINT = mapVisualProperty('edge stroke unselected paint', 
	                                                   'weight','c',
	                                                   c(min(E(g)$weight),max(E(g)$weight)),
	                                                   c(col2hex("gray"),col2hex("dark red")))
	)
	# Create a visual style.
	createVisualStyle(style.name, defaults = defaults, mappings = mappings)
	# Apply to graph.
	setVisualStyle(style.name)
	Sys.sleep(3)
	# Set NS nodes to gray.
	anyNS <- length(names(V(g))[which(V(g)$sigProt==0)]) > 0
	if (anyNS) {
	setNodePropertyBypass(
	  node.names = names(V(g))[which(V(g)$sigProt==0)],
	  new.values = col2hex("gray"),
	  visual.property = "NODE_FILL_COLOR",
	  bypass = TRUE,
	)
	setNodePropertyBypass(
	  node.names = names(V(g))[which(V(g)$sigProt==0)],
	  new.values = 200,
	  visual.property = "NODE_TRANSPARENCY",
	  bypass = TRUE,
	)
	}
	# Collect PPI edges.
	subg <- induced_subgraph(ppi_graph,vids = V(ppi_graph)[match(nodes,names(V(ppi_graph)))])
	edge_list <- apply(as_edgelist(subg, names = TRUE),1,as.list)
	# If edge list is only of length 1, unnest it to avoid problems.
	if (length(edge_list) == 1) { 
		edge_list = unlist(edge_list,recursive=FALSE)
	}
	# Add PPI edges to Cytoscape graph.
	if (length(edge_list) > 0) {
	  ppi_edges <- addCyEdges(edge_list)
	  # Add PPIs and set to black.
	  selected_edges <- selectEdges(ppi_edges,by.col = "SUID")
	  # Set to black with edge bend.
	  setEdgePropertyBypass(edge.names = selected_edges$edges,
	                        new.values = col2hex("black"),
	                        visual.property = "EDGE_STROKE_UNSELECTED_PAINT",
	                        bypass = TRUE)
	  setEdgePropertyBypass(edge.names = selected_edges$edges,
	                     new.values = TRUE,
	                     visual.property = "EDGE_BEND",
	                     bypass = TRUE)
	} # Ends IF statement.
	clearSelection()
	Sys.sleep(2)
	# Apply layout.
	layoutNetwork(network_layout)
	Sys.sleep(2)
	fitContent()
	# Save Image..
	if (!is.null(image_file)) { 
		# If image exists, first remove it.
		if (file.exists(paste(image_file,image_format,sep="."))) {
			unlink(paste(image_file,image_format,sep="."))
		}
		winfile <- gsub("/mnt/d/","D:/",image_file)
		exportImage(winfile,image_format)
	}
	# Free up some memory.
	cytoscapeFreeMemory()
}
