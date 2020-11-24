#' @keywords datagen
#' @importFrom rlang .data
#' @export
#'
#' @title Convert NWS Public Forecast Zones Shapefile.
#'
#' @param nameOnly Logical specifying whether to only return the name without
#' creating the file.
#' @param simplify Logical specifying whether to create "_05", _02" and "_01"
#' versions of the file that are simplified to 5\%, 2\% and 1\%.
#'
#' @description Create a SpatialPolygonsDataFrame for NWS weather forecast zones.
#'
#' @details A weather forecast zone shapefile is downloaded and converted to a
#' SpatialPolygonsDataFrame with additional columns of data. The resulting file
#' will be created in the spatial data directory which is set with
#' \code{setSpatialDataDir()}.
#'
#' The source data is from 2020.
#'
#' @note From the source documentation:
#'
#' The NWS issues forecasts and some watches and warnings for public zones which
#' usually are the same as counties but in many cases are subsets of counties.
#' Counties are subset into zones to allow for more accurate forecasts because
#' of the differences in weather within a county due to such things as elevation
#' or proximity to large bodies of water.
#'
#' @return Name of the dataset being created.
#'
#' @references \url{https://www.weather.gov/gis/PublicZones}
#'
#' @seealso setSpatialDataDir
#' @seealso getVariale


convertWeatherZones <- function(
  nameOnly = FALSE,
  simplify = TRUE
) {

  # ----- Setup ----------------------------------------------------------------

  # Use package internal data directory
  dataDir <- getSpatialDataDir()

  # Specify the name of the dataset and file being created
  datasetName <- 'WeatherZones'

  if (nameOnly)
    return(datasetName)

  # ----- Get the data ---------------------------------------------------------

  # Build appropriate request URL
  url <- "https://www.weather.gov/source/gis/Shapefiles/WSOM/z_03mr20.zip"

  filePath <- file.path(dataDir, basename(url))
  utils::download.file(url, filePath)
  # NOTE:  This zip file has no directory so extra subdirectory needs to be created
  utils::unzip(filePath, exdir = file.path(dataDir, 'WeatherZones'))

  # ----- Convert to SPDF ------------------------------------------------------

  # Convert shapefile into SpatialPolygonsDataFrame
  # NOTE:  The 'WeatherZones' directory has been created
  dsnPath <- file.path(dataDir, 'WeatherZones')
  shpName <- 'z_03mr20'
  SPDF <- convertLayer(
    dsn = dsnPath,
    layerName = shpName,
    encoding = 'UTF-8'
  )

  # ----- Select useful columns and rename -------------------------------------

  # > dplyr::glimpse(SPDF@data)
  # Observations: 3,853
  # Variables: 14
  # $ STATE      <chr> "GU", "GU", "GU", "GU", "GU", "GU", "GU", "GU", "GU", "GU"…
  # $ CWA        <chr> "GUM", "GUM", "GUM", "GUM", "GUM", "GUM", "GUM", "GUM", "G…
  # $ TIME_ZONE  <chr> "J", "J", "F", "G", "G", "G", "F", "G", "G", "F", "F", "K"…
  # $ FE_AREA    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
  # $ ZONE       <chr> "013", "011", "041", "021", "024", "022", "033", "023", "0…
  # $ NAME       <chr> "Kayangel", "Koror", "Pohnpei", "Yap", "Sorol", "Ngulu", "…
  # $ STATE_ZONE <chr> "GU013", "GU011", "GU041", "GU021", "GU024", "GU022", "GU0…
  # $ LON        <dbl> 134.7129, 134.5351, 158.2252, 138.1244, 140.6973, 137.5079…
  # $ LAT        <dbl> 8.0736, 7.4442, 6.8807, 9.5366, 8.2143, 8.3022, 5.4999, 10…
  # $ SHORTNAME  <chr> "Kayangel", "Koror", "Pohnpei", "Yap", "Sorol", "Ngulu", "…
  # $ InPoly_FID <chr> "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "…
  # $ SimPgnFlag <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
  # $ MaxSimpTol <dbl> 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e…
  # $ MinSimpTol <dbl> 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e…

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
  #   InPoly_FID --> (drop)
  #   SimPgnFlag --> (drop)
  #   MaxSimpTol --> (drop)
  #   MinSimpTol --> (drop)

  SPDF@data$countryCode <- "US"

  SPDF@data <-
    dplyr::select(
      .data = SPDF@data,
      countryCode = .data$countryCode,
      stateCode = .data$STATE,
      weatherForecastOffice = .data$CWA,
      zoneNumber = .data$ZONE,
      zoneName = .data$NAME,
      zoneID = .data$STATE_ZONE,
      longitude = .data$LON,
      latitude = .data$LAT
    )

  # ----- Clean SPDF -----------------------------------------------------------

  # Group polygons with the same identifier (zoneID)
  SPDF <- organizePolygons(
    SPDF,
    uniqueID = 'zoneID',
    sumColumns = c('longitude', 'latitude')
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
