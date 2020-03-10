save_network_images <- function(cys_file, file_format, ...) {
  # cys_file - path to cytoscape file.
  # file_format - output image format.
  SLEEP_TIME <- 1
  suppressPackageStartupMessages({
    library(RCy3)
  })
  # Check that we are connected to Cytoscape.
  cytoscapePing()
  # Open network file.
  openSession(file.location = cys_file)
  # Get all networks.
  networks <- getCollectionNetworks()
  # Save each network.
  message("Saving network images to file...")
  sapply(networks, function(network) {
    setCurrentNetwork(network)
    fitContent()
    Sys.sleep(SLEEP_TIME)
    exportImage(filename = getNetworkName(), type = file_format, ...)
  })
}
