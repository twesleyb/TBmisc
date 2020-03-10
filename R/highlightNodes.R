highlightNodes <- function(nodes, main.network, 
			   subnetwork.name, new.values="red") {
	suppressPackageStartupMessages({
		library(RCy3)
	})
	selectAllNodes(network = main.network)
	subnet <- createSubnetwork(network=main.network,
				   subnetwork.name=subnetwork.name)
	all_nodes <- getAllNodes(network = subnet)
	setNodePropertyBypass(node.names = all_nodes, 
			      new.values=col2hex("light gray"),
			      visual.property="NODE_FILL_COLOR")
	setNodePropertyBypass(node.names = nodes, 
			      new.values,
			      visual.property="NODE_FILL_COLOR")
	fitContent(network=subnet)
	clearSelection(type="both",network=main.network)
}
