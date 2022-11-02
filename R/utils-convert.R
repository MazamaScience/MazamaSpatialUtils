#' @keywords internal
#' @title Convert Shapefile Layer to Spatial Polygon Dataframe
#'
#' @param dsn dsn argument to code{sf::st_read()}.
#' @param layerName layer argument to code{sf::st_read()}.
#'
#' @description Raw shapefiles are read in using \code{sf::st_read()}.
#' Spatial data are reprojected onto a standard projection with CRS
#' \code{"+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"} before being returned.
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

  SFDF_projected <-
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
    )

  # Reproject
  MSU_crs <- sf::st_crs("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
  SFDF <- sf::st_transform(SFDF_projected, MSU_crs)

  # ----- Return ---------------------------------------------------------------

  return(SFDF)

}

