#' @keywords environment
#' @export
#' @title Install spatial datasets
#' @param dataset Name of spatial dataset to install.
#' @param urlBase Location of spatial data files.
#' @description Install spatial datasets found  at \code{url} into the directory
#' previously set with \code{setSpatialDataDir()}.
#'
#' If \code{pattern = NULL} (default), available datasets will be displalyed..
#'
#' @return If \code{pattern = NULL} a vector of dataset names.
#'
installSpatialData <- function(
  dataset = NULL,
  urlBase = "http://data.mazamascience.com/MazamaSpatialUtils/Spatial_0.8"
) {

  # Use package internal data directory
  dataDir <- getSpatialDataDir()

  if ( is.null(dataset) ) {

    # ----- Display all available data -----------------------------------------

    allDatasets <-
      MazamaCoreUtils::html_getLinkUrls(urlBase) %>%
      basename() %>%
      stringr::str_replace("_..\\.rda","\\.rda") %>%
      stringr::str_replace("\\.rda","") %>%
      sort() %>%
      unique()

    message(paste0(
      "Available datasets:\n",
      paste0("  ", allDatasets, collapse = "\n")
    ))

  } else {

    # ----- Get files matching pattern -----------------------------------------

    for ( version in c("", "_05", "_02", "_01") ) {

      fileName <- paste0(dataset, version, ".rda")
      filePath <- file.path(dataDir, fileName)

      if ( !file.exists(filePath) ) {

        result <- try({
          utils::download.file(paste0(urlBase, '/', fileName), filePath)
        }, silent = TRUE)

      }

    }

  }

}


