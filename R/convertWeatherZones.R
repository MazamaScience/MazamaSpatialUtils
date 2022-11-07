#' @importFrom rlang .data
#' @export
#'
#' @title Convert NWS Public Forecast Zones Shapefile.
#'
#' @description Create a simple features data frame for NWS weather forecast zones.
#'
#' The full resolution file will be named "WeatherZones.rda". In addition,
#' "_05", _02" and "_01" versions of the file will be created that that are
#' simplified to 5\%, 2\% and 1\%. Simplified versions will greatly improve the
#' speed of both searching and plotting.
#'
#' @details A weather forecast zone shapefile is downloaded and converted to a
#' simple features data frame with additional columns of data. The resulting file
#' will be created in the spatial data directory which is set with
#' \code{setSpatialDataDir()}.
#'
#' The source data is from 2022-09-13.
#'
#' @note Records with a duplicated \code{zoneID} column (typically representing
#' coastal land and its watery inlets separately) are combined so that \code{zoneID}
#' becomes a unique identifier.
#'
#' @note From the source documentation:
#'
#' The NWS issues forecasts and some watches and warnings for public zones which
#' usually are the same as counties but in many cases are subsets of counties.
#' Counties are subset into zones to allow for more accurate forecasts because
#' of the differences in weather within a county due to such things as elevation
#' or proximity to large bodies of water.
#'
#' @return Name of the datasetName being created.
#'
#' @references \url{https://www.weather.gov/gis/PublicZones}
#'

convertWeatherZones <- function() {

  # ----- Setup ----------------------------------------------------------------

  # Use package internal data directory
  dataDir <- getSpatialDataDir()

  # Specify the name of the dataset and file being created
  datasetName <- 'WeatherZones'

  # ----- Get the data ---------------------------------------------------------

  # Build appropriate request URL
  url <- "https://www.weather.gov/source/gis/Shapefiles/WSOM/z_13se22.zip"

  filePath <- file.path(dataDir, basename(url))
  utils::download.file(url, filePath)
  # NOTE:  This zip file has no directory so extra subdirectory needs to be created
  utils::unzip(filePath, exdir = file.path(dataDir, 'WeatherZones'))

  # ----- Convert to SFDF ------------------------------------------------------

  # Convert shapefile into simple features data frame
  # NOTE:  The 'WeatherZones' directory has been created
  dsnPath <- file.path(dataDir, 'WeatherZones')
  shpName <- 'z_13se22'
  SFDF <- convertLayer(
    dsn = dsnPath,
    layer = shpName
  )

  # ----- Select useful columns and rename -------------------------------------

  # > dplyr::glimpse(SFDF, width = 75)
  # Rows: 3,996
  # Columns: 11
  # $ STATE      <chr> "AL", "AL", "AL", "AL", "AL", "AL", "AL", "AL", "AL", …
  # $ CWA        <chr> "BMX", "MOB", "BMX", "BMX", "BMX", "BMX", "HUN", "BMX"…
  # $ TIME_ZONE  <chr> "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C",…
  # $ FE_AREA    <chr> "ec", "sc", "se", "cc", "cc", "se", "ne", "ne", "cc", …
  # $ ZONE       <chr> "019", "057", "046", "017", "034", "050", "009", "020"…
  # $ NAME       <chr> "Calhoun", "Butler", "Bullock", "Blount", "Bibb", "Bar…
  # $ STATE_ZONE <chr> "AL019", "AL057", "AL046", "AL017", "AL034", "AL050", …
  # $ LON        <dbl> -85.8261, -86.6803, -85.7161, -86.5674, -87.1264, -85.…
  # $ LAT        <dbl> 33.7714, 31.7524, 32.1005, 33.9809, 32.9986, 31.8696, …
  # $ SHORTNAME  <chr> "Calhoun", "Butler", "Bullock", "Blount", "Bibb", "Bar…
  # $ geometry   <MULTIPOLYGON [°]> MULTIPOLYGON (((-85.5301 33..., MULTIPOLY…

  # Data Dictionary:
  #   STATE -------> stateCode: 2-character postal code
  #   CWA ---------> weatherForecastOffice: CWA abbreviation for office
  #   TIME_ZONE ---> (drop)
  #   FE_AREA -----> (drop)
  #   ZONE --------> zoneNumber: zone number
  #   NAME --------> name: zone name
  #   STATE_ZONE --> zoneID: unqiue identifier
  #   LON ---------> longitude: longitude of zone centroid
  #   LAT ---------> latitude: latitude of zone centroid
  #   SHORTNAME ---> (drop)


  SFDF$countryCode <- "US"

  SFDF <-
    SFDF %>%
    dplyr::select(
      countryCode = .data$countryCode,
      stateCode = .data$STATE,
      weatherForecastOffice = .data$CWA,
      zoneNumber = .data$ZONE,
      zoneName = .data$NAME,
      zoneID = .data$STATE_ZONE,
      longitude = .data$LON,
      latitude = .data$LAT
    )

  # ----- Combine polygons -----------------------------------------------------

  # NOTE:  Some zoneIDs are duplicated, e.g. "MD018" which is used for the land
  # NOTE:  part of a peninsula in Maryland as well as the water inlets. Presumablyl
  # NOTE:  This is so the NWS can issue separate land and maritime forecasts for
  # NOTE:  users.
  # NOTE:
  # NOTE:  We combine them here.

  copy_fields <- setdiff(names(SFDF), c("geometry"))

  SFDF <-
    SFDF %>%
    MazamaSpatialUtils::dissolve(
      "zoneID",
      copy_fields = copy_fields
    )

  # ----- Simplify and save ----------------------------------------------------

  uniqueIdentifier <- "zoneID"

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

}
