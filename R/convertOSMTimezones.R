#' @keywords datagen
#' @importFrom rlang .data
#' @export
#'
#' @title Convert OSM timezone shapefile
#'
#' @description Create a simple features data frame for world timezones.
#'
#' The full resolution file will be named "OSMTimezones.rda". In addition,
#' "_05", _02" and "_01" versions of the file will be created that that are
#' simplified to 5\%, 2\% and 1\%. Simplified versions will greatly improve the
#' speed of both searching and plotting.
#'
#' @details A world timezone shapefile is downloaded and converted to a
#' simple features data frame with additional columns of data. The resulting file
#' will be created in the spatial data directory which is set with
#' \code{setSpatialDataDir()}.
#'
#' There are 2 timezones which have polygons but the associated rows in the dataframe have no data.
#' These timezones also have no \code{countryCode} assigned. We hope to rectify this in a future release.
#' These are the missing timezones:
#' \preformatted{
#' > OSMTimezones$timezone[is.na(OSMTimezones$countryCode)]
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
#' @return Name of the datasetName being created.
#'
#' @references \url{https://github.com/evansiroky/timezone-boundary-builder}
#'

convertOSMTimezones <- function() {

  # ----- Setup ----------------------------------------------------------------

  # Use package internal data directory
  dataDir <- getSpatialDataDir()

  # Specify the name of the dataset and file being created
  datasetName <- 'OSMTimezones'

  # ----- Get the data ---------------------------------------------------------

  # Build appropriate request URL
  url <- "https://github.com/evansiroky/timezone-boundary-builder/releases/download/2022f/timezones.shapefile.zip"

  filePath <- file.path(dataDir, basename(url))
  utils::download.file(url, filePath)
  # NOTE:  This zip file has no directory so extra subdirectory needs to be created
  utils::unzip(filePath, exdir = file.path(dataDir, 'OSMTimezones'))

  # ----- Convert to SFDF ------------------------------------------------------

  # Convert shapefile into simple features data frame
  # NOTE:  The 'OSMTimezones' directory has been created
  dsnPath <- file.path(dataDir, 'OSMTimezones')
  shpName <- 'combined-shapefile'
  SFDF <- convertLayer(
    dsn = dsnPath,
    layer = shpName
  )

  # ----- Select useful columns and rename -------------------------------------

  # > dplyr::glimpse(SFDF, width = 75)
  # Rows: 426
  # Columns: 2
  # $ tzid     <chr> "Africa/Abidjan", "Africa/Accra", "Africa/Addis_Ababa", …
  # $ geometry <MULTIPOLYGON [°]> MULTIPOLYGON (((-5.440683 4..., MULTIPOLYGO…

  # Data Dictionary:
  #   tzid --------> timezone: The name of the timezone

  # Get additional data from Wikipedia
  wikipediaTimezoneTable <- convertWikipediaTimezoneTable()

  # Merge the additional data onto the SFDF
  SFDF <- dplyr::left_join(SFDF, wikipediaTimezoneTable, by = c('tzid' = 'timezone'))

  # > dplyr::glimpse(SFDF, width = 75)
  # Rows: 426
  # Columns: 9
  # $ tzid                      <chr> "Africa/Abidjan", "Africa/Accra", "Afri…
  # $ countryCode               <chr> "CI", "GH", "ET", "DZ", "ER", "ML", "CF…
  # $ countryCodes              <chr> "CI, BF, GH, GM, GN, IS, ML, MR, SH, SL…
  # $ timezone_STD_abbreviation <chr> "GMT", "GMT", "EAT", "CET", "EAT", "GMT…
  # $ timezone_DST_abbreviation <chr> "GMT", "GMT", "EAT", "CET", "EAT", "GMT…
  # $ UTC_STD_offset            <dbl> 0, 0, 3, 1, 3, 0, 1, 0, 0, 2, 1, 2, 2, …
  # $ UTC_DST_offset            <dbl> 0, 0, 3, 1, 3, 0, 1, 0, 0, 2, 1, 2, 2, …
  # $ notes                     <chr> "", "Link to Africa/Abidjan", "Link to …
  # $ geometry                  <MULTIPOLYGON [°]> MULTIPOLYGON (((-5.440683 …

  # Create the new dataframe in a specific column order
  SFDF <-
    SFDF %>%
    dplyr::rename(
      timezone = .data$tzid
    )

  # ----- Simplify and save ----------------------------------------------------

  uniqueIdentifier <- "claimants"

  simplifyAndSave(
    SFDF = SFDF,
    datasetName = datasetName,
    uniqueIdentifier = uniqueIdentifier,
    dataDir = dataDir
  )

  # ----- Clean up and return --------------------------------------------------

  # Clean up
  unlink(filePath, force = TRUE)
  unlink(dsnPath, recursive = TRUE, force = TRUE)

  return(invisible(datasetName))

}

# ===== TEST ===================================================================

if ( FALSE ) {

  library(sf)

  # Look or horizontal lines from polygons that cross the dateline.
  # NOTE:  These are sometimes created by sf::st_make_valid()
  loadSpatialData(datasetName)
  SFDF <- get(paste0(datasetName, ""))
  SFDF_05 <- get(paste0(datasetName, "_05"))
  SFDF_02 <- get(paste0(datasetName, "_02"))
  SFDF_01 <- get(paste0(datasetName, "_01"))

  plot(SFDF_01$geometry)
  dev.off(dev.list()["RStudioGD"])
  plot(SFDF_02$geometry)
  dev.off(dev.list()["RStudioGD"])
  plot(SFDF_05$geometry)
  dev.off(dev.list()["RStudioGD"])
  #plot(SFDF$geometry)

  # Try out getSpatialData()
  lons <- c(-120:-110, 0:10)
  lats <- c(30:40, 30:40)

  df <- getSpatialData(lons, lats, SFDF_01)
  df <- getSpatialData(lons, lats, SFDF_02)
  df <- getSpatialData(lons, lats, SFDF_05)
  df <- getSpatialData(lons, lats, SFDF)

  # Special Case of Russian failing to plot properly
  SFDF %>% dplyr::filter(countryCode == "RU") %>% sf::st_geometry() %>% plot()

}
