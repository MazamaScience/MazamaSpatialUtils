#' @export
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
convertLayer <- function(
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


#' @export
#' @title Save simplified versions of a spatial features dataframe
#'
#' @param SFDF Simple features dataframe.
#' @param datasetName Base name used for \code{SFDF}.
#' @param uniqueIdentifier Name of column containing unique polygon identifiers.
#' @param dataDir Spatial data directory set with \code{setSpatialDataDir()}.
#'
#' @description Creates and saves  "_05", _02" and "_01"
#' versions of \code{SFDF} that are simplified to 5\%, 2\% and 1\%.
#' .
#' @return Writes data files to disk.
#'
simplifyAndSave <- function(
    SFDF = NULL,
    datasetName = NULL,
    uniqueIdentifier = NULL,
    dataDir = getSpatialDataDir()
) {

  # ----- Validate Parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(SFDF)
  MazamaCoreUtils::stopIfNull(datasetName)
  MazamaCoreUtils::stopIfNull(uniqueIdentifier)

  # ----- Add polygonID --------------------------------------------------------

  # Guarantee that all polygons are unique
  if ( any(duplicated(SFDF[[uniqueIdentifier]])) )
    stop(sprintf("Column '%s' has multiple records. An organizePolygons() step is needed.", uniqueIdentifier))

  # All polygons are unique so we can add a polygonID
  SFDF$polygonID <- as.character(seq_len(nrow(SFDF)))

  # ----- Simplify, correct and save -------------------------------------------

  # NOTE:  The rmapshaper::ms_simplify() function has, IMHO, the best simplification algorithm.
  # NOTE:
  # NOTE:  After simplification, we use set_make_valid() to correct problems with
  # NOTE:  individual polygon geometries and st_wrap_dateline() to make sure that
  # NOTE:  polygons that cross the dateline are split into two, thus avoiding
  # NOTE:  big horizontal lines when plotting with the default projection.
  # NOTE:
  # NOTE:  We create the simplified versions first because that sometimes
  # NOTE:  fixes problems with invalid geometries and thus removes the need to
  # NOTE:  st_mak_valid() which messes up polygons that cross the dateline.

  # TODO:  On 2022-11-07, the st_wrap_dateline() function isn't working with the
  # TODO:  combination of libraries and packages on my Mac. See the relevant issue:
  # TODO:    https://github.com/r-spatial/sf/issues/1999
  # TODO:
  # TODO:  For now, we will only make_valid() if necessary and skip wrap_dateline().

  # ----- 1% -------------------------------------------------------------------

  message("\nSimplifying to 1%...")
  SFDF_01 <- rmapshaper::ms_simplify(SFDF, 0.01)
  if ( any(!sf::st_is_valid(SFDF_01)) ) {
    message("Correcting invalid geometries...")
    SFDF_01 <- SFDF_01 %>% sf::st_make_valid()
    ###SFDF_01 <- SFDF_01 %>% sf::st_make_valid() %>% sf::st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))
  }
  datasetName_01 <- paste0(datasetName, "_01")
  message("Saving 1% version...")
  assign(datasetName_01, SFDF_01)
  save(list = datasetName_01, file = paste0(dataDir,"/", datasetName_01, '.rda'))
  rm(list = c("SFDF_01", datasetName_01))

  # ----- 2% -------------------------------------------------------------------

  message("\nSimplifying to 2%...")
  SFDF_02 <- rmapshaper::ms_simplify(SFDF, 0.02)
  if ( any(!sf::st_is_valid(SFDF_02)) ) {
    message("Correcting invalid geometries...")
    SFDF_02 <- SFDF_02 %>% sf::st_make_valid()
    ###SFDF_02 <- SFDF_02 %>% sf::st_make_valid() %>% sf::st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))
  }
  datasetName_02 <- paste0(datasetName, "_02")
  message("Saving 2% version...")
  assign(datasetName_02, SFDF_02)
  save(list = datasetName_02, file = paste0(dataDir,"/", datasetName_02, '.rda'))
  rm(list = c("SFDF_02", datasetName_02))

  # ----- 5% -------------------------------------------------------------------

  message("\nSimplifying to 5%...")
  SFDF_05 <- rmapshaper::ms_simplify(SFDF, 0.05)
  if ( any(!sf::st_is_valid(SFDF_05)) ) {
    message("Correcting invalid geometries...")
    SFDF_05 <- SFDF_05 %>% sf::st_make_valid()
    ###SFDF_05 <- SFDF_05 %>% sf::st_make_valid() %>% sf::st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))
  }
  datasetName_05 <- paste0(datasetName, "_05")
  message("Saving 5% version...")
  assign(datasetName_05, SFDF_05)
  save(list = datasetName_05, file = paste0(dataDir,"/", datasetName_05, '.rda'))
  rm(list = c("SFDF_05", datasetName_05))

  # ----- Full Resolution ------------------------------------------------------

  message("\nFull resolution version...")
  if ( any(!sf::st_is_valid(SFDF)) ) {
    message("Correcting invalid geometries...")
    SFDF <- SFDF %>% sf::st_make_valid()
    ###SFDF <- SFDF %>% sf::st_make_valid() %>% sf::st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))
  }
  message("Saving full resolution version...")
  assign(datasetName, SFDF)
  save(list = c(datasetName), file = paste0(dataDir, '/', datasetName, '.rda'))
  rm(list = datasetName)

}
