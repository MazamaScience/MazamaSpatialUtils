#' @keywords environment
#' @export
#' @title Install Spatial Datasets
#' @param url location of spatial data .tar.gz file
#' @description Install spatial datasets found  at \code{url} into the directory previously 
#' set with \code{setSpatialDataDir()}.
#' 
#' @return Nothing.
#' @seealso setSpatialDataDir
#' @seealso loadSpatialData
installSpatialData <- function(url="http://mazamascience.com/RData/mazama_spatial_files.tar.gz") {
  
  # Use package internal data directory
  dataDir <- getSpatialDataDir()
  
  tempfile <- base::tempfile("spatial_data", fileext=".tar.gz")
  utils::download.file(url, tempfile)
  utils::untar(tempfile, exdir=dataDir)
  base::file.remove(tempfile)
  
}

