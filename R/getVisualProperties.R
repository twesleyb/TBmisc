getVisualProperties <- function(property = NULL) {
  visual.properties <- getVisualPropertyNames()
  graph.properties <- lapply(visual.properties, getVisualPropertyDefault)
  names(graph.properties) <- visual.properties
  if (!is.null(property)) {
    idx <- grep(toupper(property), names(graph.properties))
    return(graph.properties[idx])
  } else {
    return(graph.properties)
  }
}
