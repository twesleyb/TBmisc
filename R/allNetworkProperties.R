# Get all network properties and their default.
allNetworkProperties <- function(...) {
  suppressPackageStartupMessages({
    library(RCy3)
  })
  visual.properties <- getVisualPropertyNames()
  all.properties <- list()
  for (i in seq_along(visual.properties)) {
    vp <- emptyVisualProperty()
    vp$visual.prop <- gsub("_", " ", tolower(names(all.properties)[i]))
    vp$visual.prop.values <- getVisualPropertyDefault(visual.properties[i])
    all.properties[[i]] <- vp
  }
  names(all.properties) <- visual.properties
  return(all.properties)
}
