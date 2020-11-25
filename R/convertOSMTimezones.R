#' @keywords datagen
#' @importFrom rlang .data
#' @export
#'
#' @title Convert OSM Timezone Shapefile
#'
#' @param nameOnly Logical specifying whether to only return the name without
#' creating the file.
#' @param simplify Logical specifying whether to create "_05", _02" and "_01"
#' versions of the file that are simplified to 5\%, 2\% and 1\%.
#'
#' @description Create a SpatialPolygonsDataFrame for world timezones.
#'
#' @details A world timezone shapefile is downloaded and converted to a
#' SpatialPolygonsDataFrame with additional columns of data. The resulting file
#' will be created in the spatial data directory which is set with
#' \code{setSpatialDataDir()}.
#'
#' There are 2 timezones which have polygons but the associated rows in the dataframe have no data.
#' These timezones also have no \code{countryCode} assigned. We hope to rectify this in a future release.
#' These are the missing timezones:
#' \preformatted{
#' > OSMTimezones@data$timezone[is.na(OSMTimezones$countryCode)]
#'  [1] "America/Nuuk"  "Asia/Qostanay"
#' }
#'
#' @note From the source documentation:
#'
#' This project aims to stay up-to-date with all of the currently valid
#' timezones that are defined in the timezone database. This project also will
#' attempt to provide the most accurate possible boundaries of timezones
#' according to community input.
#'
#' The underlying data is downloaded from OpenStreetMap via the overpass turbo
#' API. Various boundaries are assembled together to produce each zone with
#' various geographic operations. In numerous edge cases arbitrary boundaries
#' get created in various zones which are noted in the timezones.json file.
#'
#' To maintain consistency with the timezone database, this project will only
#' create a new release after the timezone database creates a new release. If
#' there are no new timezones created or deleted in a timezone database release,
#' then this project will only create a release if there have been changes
#' performed to the boundary definitions of an existing zone within this project.
#'
#' @return Name of the dataset being created.
#'
#' @references \url{https://github.com/evansiroky/timezone-boundary-builder}
#'
#' @seealso setSpatialDataDir
#' @seealso getVariable

convertOSMTimezones <- function(
  nameOnly = FALSE,
  simplify = TRUE
) {

  # ----- Setup ----------------------------------------------------------------

  # Use package internal data directory
  dataDir <- getSpatialDataDir()

  # Specify the name of the dataset and file being created
  datasetName <- 'OSMTimezones'

  if (nameOnly)
    return(datasetName)

  # ----- Get the data ---------------------------------------------------------

  # Build appropriate request URL
  url <- "https://github.com/evansiroky/timezone-boundary-builder/releases/download/2020a/timezones.shapefile.zip"

  filePath <- file.path(dataDir, basename(url))
  utils::download.file(url, filePath)
  # NOTE:  This zip file has no directory so extra subdirectory needs to be created
  utils::unzip(filePath, exdir = file.path(dataDir, 'OSMTimezones'))

  # ----- Convert to SPDF ------------------------------------------------------

  # Convert shapefile into SpatialPolygonsDataFrame
  # NOTE:  The 'OSMTimezones' directory has been created
  dsnPath <- file.path(dataDir, 'OSMTimezones/dist')
  shpName <- 'combined-shapefile'
  SPDF <- convertLayer(
    dsn = dsnPath,
    layerName = shpName,
    encoding = 'UTF-8'
  )

  # ----- Select useful columns and rename -------------------------------------

  # > dplyr::glimpse(SPDF@data)
  # Observations: 426
  # Variables: 1
  # $ tzid <chr> "Africa/Abidjan", "Africa/Accra", "Africa/Addis_Ababa", "Africa/…

  # Data Dictionary:
  #   tzid --------> timezone: The name of the timezone

  # Get additional data from Wikipedia
  wikipediaTimezoneTable <- convertWikipediaTimezoneTable()

  # Merge the additional data onto the @data slot of the SPDF
  SPDF@data <- dplyr::left_join(SPDF@data, wikipediaTimezoneTable, by = c('tzid' = 'timezone'))

  # > dplyr::glimpse(SPDF@data)
  # Rows: 427
  # Columns: 6
  # $ tzid           <chr> "Africa/Abidjan", "Africa/Accra", "Africa/Addis_Ababa", "Af…
  # $ UTC_offset     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
  # $ UTC_DST_offset <dbl> 0, 0, 3, 1, 3, 0, 1, 0, 0, 2, 1, 2, 2, 1, 1, 0, 0, 3, 3, 1,…
  # $ countryCode    <chr> "CI", "GH", "ET", "DZ", "ER", "ML", "CF", "GM", "GW", "MW",…
  # $ longitude      <dbl> NA, NA, 38.70000, 3.05000, 38.88333, NA, 18.58333, NA, NA, …
  # $ latitude       <dbl> NA, NA, 9.0338889, 36.7963889, 15.3388889, NA, 4.3727778, N…

  # Create the new dataframe in a specific column order
  SPDF@data <-
    dplyr::select(
      .data = SPDF@data,
      timezone = .data$tzid,
      countryCode = .data$countryCode,
      UTC_offset = .data$UTC_DST_offset,
      longitude = .data$longitude,
      latitude = .data$latitude
    )

  # ----- Clean SPDF -----------------------------------------------------------

  # Group polygons with the same identifier (timezone)
  SPDF <- organizePolygons(
    SPDF,
    uniqueID = 'timezone',
    sumColumns = NULL
  )

  # Clean topology errors
  if ( !cleangeo::clgeo_IsValid(SPDF) ) {
    SPDF <- cleangeo::clgeo_Clean(SPDF)
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
