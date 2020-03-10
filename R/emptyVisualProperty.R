# Empty visual property.
emptyVisualProperty <- function(...) {
  visual.property <- list(
    visual.prop = NULL,
    table.column = NULL,
    mapping.type = NULL,
    table.column.values = NULL,
    visual.prop.values = NULL
  )
  return(visual.property)
}
