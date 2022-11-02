#' @keywords datagen
#' @importFrom rlang .data
#' @export
#'
#' @title Convert Timezone Shapefile
#'
#' @param nameOnly Logical specifying whether to only return the name without
#' creating the file.
#' @param simplify Logical specifying whether to create "_05", _02" and "_01"
#' versions of the file that are simplified to 5\%, 2\% and 1\%.
#'
#' @description Create a SpatialPolygonsDataFrame for world timezones.
#'
#' @details A world timezones shapefile is downloaded and converted to a
#' SpatialPolygonsDataFrame with additional columns of data. The resulting file
#' will be created in the spatial data directory which is set with
#' \code{setSpatialDataDir()}.
#'
#' The source data is from 2020 --
#' \href{https://github.com/evansiroky/timezone-boundary-builder/releases/tag/2020d}{release 2020d}.
#'
#' @note From the source documentation:
#'
#' Each shape or geojson object has a single attribute or property respectively
#' called tzid. The tzid corresponds to the timezone name as defined in the
#' timezone database (for example: America/Los_Angeles or Asia/Shanghai).
#'
#' This project aims to stay up-to-date with all of the currently valid timezones
#' that are defined in the timezone database. This project also will attempt to
#' provide the most accurate possible boundaries of timezones according to
#' community input.
#'
#' The underlying data is downloaded from OpenStreetMap via the overpass turbo
#' API. Various boundaries are assembled together to produce each zone with
#' various geographic operations.
# '
#' @return Name of the dataset being created.
#'
#' @references \url{https://github.com/evansiroky/timezone-boundary-builder}
#'
#' @seealso setSpatialDataDir
#' @seealso getTimezone
#' @seealso convertWikipediaTimezoneTable

convertWorldTimezones <- function(
  nameOnly = FALSE,
  simplify = TRUE
) {

  # ----- Setup ----------------------------------------------------------------

  # Use package internal data directory
  dataDir <- getSpatialDataDir()

  # Specify the name of the dataset and file being created
  datasetName <- 'WorldTimezones'

  if (nameOnly)
    return(datasetName)

  # ----- Get the data ---------------------------------------------------------

  # Build appropriate request URL
  url <- "https://github.com/evansiroky/timezone-boundary-builder/releases/download/2020d/timezones.shapefile.zip"

  filePath <- file.path(dataDir, basename(url))
  utils::download.file(url, filePath)
  # NOTE:  This zip file has no directory so an extra subdirectory needs to be created
  utils::unzip(filePath, exdir = file.path(dataDir, 'timezones'))

  # ----- Convert to SPDF ------------------------------------------------------

  # Convert shapefile into SpatialPolygonsDataFrame
  # NOTE:  The 'timezones' directory has been created
  dsnPath <- file.path(dataDir, 'timezones')
  shpName <- 'combined-shapefile'
  SPDF <- convertLayer(
    dsn = dsnPath,
    layerName = shpName,
    encoding = 'UTF-8'
  )

  # ----- Merge with timezone table --------------------------------------------

  # Rename "TZID" to "timezone"
  names(SPDF@data) <- c('timezone')

  # Now get additional data from Wikipedia
  wikipediaTimezoneTable <- convertWikipediaTimezoneTable()

  # Merge the additional data onto the @data slot of the SPDF
  SPDF@data <- dplyr::left_join(SPDF@data, wikipediaTimezoneTable, by = "timezone")

  # > dplyr::glimpse(SPDF@data, width = 75)
  # Rows: 426
  # Columns: 8
  # $ timezone       <chr> "Africa/Abidjan", "Africa/Accra", "Africa/Addis_A…
  # $ UTC_offset     <dbl> 0, 0, 3, 1, 3, 0, 1, 0, 0, 2, 1, 2, 2, 1, 1, 0, 0…
  # $ UTC_DST_offset <dbl> 0, 0, 3, 1, 3, 0, 1, 0, 0, 2, 1, 2, 2, 0, 2, 0, 0…
  # $ countryCode    <chr> "CI", "GH", "ET", "DZ", "ER", "ML", "CF", "GM", "…
  # $ longitude      <dbl> -4.0333333, -0.2166667, 38.7000000, 3.0500000, 38…
  # $ latitude       <dbl> 5.321944, 5.559167, 9.033889, 36.796389, 15.33888…
  # $ status         <chr> "Canonical", "Canonical", "Alias", "Canonical", "…
  # $ notes          <chr> "", "", "Link to Africa/Nairobi", "", "Link to Af…

  # ----- Clean SPDF -----------------------------------------------------------

  # Group polygons with the same identifier (stateFIPS)
  SPDF <- organizePolygons(
    SPDF,
    uniqueID = 'timezone',
    sumColumns = NULL
  )

  # Clean topology errors
  if ( !cleangeo::clgeo_IsValid(SPDF) ) {
    SPDF <- cleangeo::clgeo_Clean(SPDF, verbose = TRUE)
  }

  # ----- Name and save the data -----------------------------------------------

  # Assign a name and save the data
  message("Saving full resolution version...\n")
  assign(datasetName, SPDF)
  save(list = c(datasetName), file = paste0(dataDir, '/', datasetName, '.rda'))
  rm(list = datasetName)

  # ----- Simplify -------------------------------------------------------------

  if ( simplify ) {
    # Create new, simplified datsets: one with 5%, 2%, and one with 1% of the vertices of the original
    # NOTE:  This may take several minutes.
    message("Simplifying to 5%...\n")
    SPDF_05 <- rmapshaper::ms_simplify(SPDF, 0.05)
    SPDF_05@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
    # Clean topology errors
    if ( !cleangeo::clgeo_IsValid(SPDF_05) ) {
      SPDF_05 <- cleangeo::clgeo_Clean(SPDF_05)
    }
    datasetName_05 <- paste0(datasetName, "_05")
    message("Saving 5% version...\n")
    assign(datasetName_05, SPDF_05)
    save(list = datasetName_05, file = paste0(dataDir,"/", datasetName_05, '.rda'))
    rm(list = c("SPDF_05",datasetName_05))

    message("Simplifying to 2%...\n")
    SPDF_02 <- rmapshaper::ms_simplify(SPDF, 0.02)
    SPDF_02@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
    # Clean topology errors
    if ( !cleangeo::clgeo_IsValid(SPDF_02) ) {
      SPDF_02 <- cleangeo::clgeo_Clean(SPDF_02)
    }
    datasetName_02 <- paste0(datasetName, "_02")
    message("Saving 2% version...\n")
    assign(datasetName_02, SPDF_02)
    save(list = datasetName_02, file = paste0(dataDir,"/", datasetName_02, '.rda'))
    rm(list = c("SPDF_02",datasetName_02))

    message("Simplifying to 1%...\n")
    SPDF_01 <- rmapshaper::ms_simplify(SPDF, 0.01)
    SPDF_01@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
    # Clean topology errors
    if ( !cleangeo::clgeo_IsValid(SPDF_01) ) {
      SPDF_01 <- cleangeo::clgeo_Clean(SPDF_01)
    }
    datasetName_01 <- paste0(datasetName, "_01")
    message("Saving 1% version...\n")
    assign(datasetName_01, SPDF_01)
    save(list = datasetName_01, file = paste0(dataDir,"/", datasetName_01, '.rda'))
    rm(list = c("SPDF_01",datasetName_01))
  }

  # ----- Clean up and return --------------------------------------------------

  # Clean up
  unlink(filePath, force = TRUE)
  unlink(dsnPath, recursive = TRUE, force = TRUE)

  return(invisible(datasetName))

}

