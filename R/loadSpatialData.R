#' @export
#' @importFrom rlang .data
#'
#' @title Load spatial datasets
#'
#' @param pattern Regular expression used to match file names.
#'
#' @description Load datasets found in the directory previously set with
#' \code{setSpatialDataDir()}.
#' Only files matching \code{pattern} will be loaded.
#' By default, all \code{.rda} files are matched.
#'
#' Core datastes available for the package include:
#' \itemize{
#' \item{\code{TMWorldBorders} -- high resolution country polygons (higher
#' resolution than \code{SimpleCountries})}
#' \item{\code{NaturalEarthAdm1} -- state/province polygons throughout the world}
#' \item{\code{USCensusCounties} -- county polygons in the United States}
#' \item{\code{WorldTimezones} -- high resolution timezone polygons (higher
#' resolution than \code{SimpleTimezones})}
#' }
#'
#' These can be installed with \code{installSpatialData()}.
#'
#' @return Invisibly returns a vector of spatial dataset names loaded into the
#' global environment.
#'
#' @seealso setSpatialDataDir
#' @seealso installSpatialData
#'
loadSpatialData <- function(
  pattern = "*\\.rda"
) {

  # Use package internal data directory
  dataDir <- getSpatialDataDir()

  # Find paths that match the pattern
  filePaths <- list.files(dataDir, pattern, full.names = TRUE)

  # Exclude directories
  filePaths <- filePaths[!dir.exists(filePaths)]

  if ( length(filePaths) == 0 ) {

    stop(paste0('No files matching "',
                pattern,
                '" found in ',
                dataDir,
                '.'),
         call. = FALSE)

  } else {

    for ( filePath in filePaths ) {
      load(filePath, envir = .GlobalEnv)
    }

    # Return names of all SFDF loaded into the global environment
    names <-
      base::basename(filePaths) %>%
      stringr::str_replace("\\.rda", "") %>%
      unique() %>%
      sort()

    return(invisible(names))

  }

}

