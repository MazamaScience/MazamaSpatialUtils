#' @keywords environment
#' @export
#' @title Install version 0.6 spatial datasets
#' @param urlBase location of spatial data files
#' @param file name of the tar.gz file containing spatial datasets
#' @description Install spatial datasets found  at \code{url} into the directory
#' previously set with \code{setSpatialDataDir()}.
#'
#' @return Nothing.
installSpatialData_0.6 <- function(
  urlBase = "http://data.mazamascience.com/MazamaSpatialUtils/Spatial",
  file = "mazama_spatial_files-0.6.tar.gz"
) {

  # Use package internal data directory
  dataDir <- getSpatialDataDir()

  tempfile <- base::tempfile("spatial_data", fileext = ".tar.gz")
  utils::download.file(paste0(urlBase, '/', file), tempfile)
  utils::untar(tempfile, exdir = dataDir)
  base::file.remove(tempfile)

}

