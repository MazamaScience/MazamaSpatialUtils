#' @keywords internal
#' @title Convert shapefile layer to simple features dataframe
#'
#' @param dsn dsn argument to \code{sf::st_read()}.
#' @param layer layer argument to \code{sf::st_read()}.
#'
#' @description Raw shapefiles are read in using \code{\link[sf]{st_read}}.
#' Spatial data are reprojected onto a standard projection
#' (\url{https://epsg.io/4269}) before being returned.
#'
#' If shapefiles have no projection information they are assumed to 'geographic'
#' coordinates
#' .
#' @return An object of class \code{sf}.
#'
.convertLayer <- function(
  dsn = NULL,
  layer
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(dsn)

  dsn <- path.expand(dsn)

  if ( !file.exists(dsn) )
    stop(sprintf("file not found: %s", dsn))

  # ----- Read in data ---------------------------------------------------------

  SFDF <-

    # Read in data on its native projection
    sf::st_read(
      dsn,
      layer,
      ###...,
      query = NA,
      options = NULL,
      quiet = FALSE,
      geometry_column = 1L,
      type = 0,
      promote_to_multi = TRUE,
      stringsAsFactors = FALSE,                     # ensure FALSE
      int64_as_string = FALSE,
      check_ring_dir = TRUE,                        # change from default
      fid_column_name = character(0),
      drivers = character(0),
      wkt_filter = character(0),
      optional = FALSE
    ) %>%

    # Reproject to North America projection: https://epsg.io/4269
    sf::st_transform(sf::st_crs(4269))

  # ----- Return ---------------------------------------------------------------

  return(SFDF)

}


#' @keywords internal
#' @title Save simplified versions of a spatial features dataframe
#'
#' @param SFDF Simple features dataframe.
#' @param datasetName Base name used for \code{SFDF}.
#' @param dataDir Spatial data directory set with \code{setSpatialDataDir()}.
#'
#' @description Creates and saves  "_05", _02" and "_01"
#' versions of \code{SFDF} that are simplified to 5\%, 2\% and 1\%.
#' .
#' @return Writes data files to disk.
#'
.simplifyAndSave <- function(
    SFDF = NULL,
    datasetName = NULL,
    dataDir = getSpatialDataDir(),
    makeValid = TRUE
) {

  message("Simplifying to 5%...\n")
  SFDF_05 <- rmapshaper::ms_simplify(SFDF, 0.05)
  if ( makeValid )
    SFDF_05 <- sf::st_make_valid(SFDF_05)
  datasetName_05 <- paste0(datasetName, "_05")
  message("Saving 5% version...\n")
  assign(datasetName_05, SFDF_05)
  save(list = datasetName_05, file = paste0(dataDir,"/", datasetName_05, '.rda'))
  rm(list = c("SFDF_05", datasetName_05))

  message("Simplifying to 2%...\n")
  SFDF_02 <- rmapshaper::ms_simplify(SFDF, 0.02)
  if ( makeValid )
    SFDF_02 <- sf::st_make_valid(SFDF_02)
  datasetName_02 <- paste0(datasetName, "_02")
  message("Saving 2% version...\n")
  assign(datasetName_02, SFDF_02)
  save(list = datasetName_02, file = paste0(dataDir,"/", datasetName_02, '.rda'))
  rm(list = c("SFDF_02", datasetName_02))

  message("Simplifying to 1%...\n")
  SFDF_01 <- rmapshaper::ms_simplify(SFDF, 0.01)
  if ( makeValid )
    SFDF_01 <- sf::st_make_valid(SFDF_01)
  datasetName_01 <- paste0(datasetName, "_01")
  message("Saving 1% version...\n")
  assign(datasetName_01, SFDF_01)
  save(list = datasetName_01, file = paste0(dataDir,"/", datasetName_01, '.rda'))
  rm(list = c("SFDF_01", datasetName_01))

}
