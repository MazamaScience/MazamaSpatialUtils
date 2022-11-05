#' @keywords datagen
#' @importFrom rlang .data
#' @export
#'
#' @title Convert NWS Public Forecast Zones Shapefile
#'
#' @param nameOnly Logical specifying whether to only return the name without
#' creating the file.
#' @param simplify Logical specifying whether to create "_05", _02" and "_01"
#' versions of the file that are simplified to 5\%, 2\% and 1\%.
#'
#' @description Create a simple features data frame for NWS weather forecast zones
#'
#' @details A NWS forecast zone shapefile is downloaded and converted to a
#' simple features data frame with additional columns of data. The resulting file
#' will be created in the spatial data directory which is set with
#' \code{setSpatialDataDir()}.
#'
#' The source data is from 2022-09-13.
#'
#' @note From the source documentation:
#'
#' This data set is used to delineate the Fire Weather Zones that are used by
#' NWS in the fire weather forecast program.
#'
#' @return Name of the datasetName being created.
#'
#' @references \url{https://www.weather.gov/gis/FireZones}
#'
#' @seealso setSpatialDataDir
#' @seealso getVariable

convertNWSFireZones <- function(
  nameOnly = FALSE,
  simplify = TRUE
) {

  # ----- Setup ----------------------------------------------------------------

  # Use package internal data directory
  dataDir <- getSpatialDataDir()

  # Specify the name of the dataset and file being created
  datasetName <- 'NWSFireZones'

  if (nameOnly)
    return(datasetName)

  # ----- Get the data ---------------------------------------------------------

  # Build appropriate request URL
  url <- "https://www.weather.gov/source/gis/Shapefiles/WSOM/fz13se22.zip"

  filePath <- file.path(dataDir, basename(url))
  utils::download.file(url, filePath)
  # NOTE:  This zip file has no directory so extra subdirectory needs to be created
  utils::unzip(filePath, exdir = file.path(dataDir, 'NWSFireZones'))

  # ----- Convert to SFDF ------------------------------------------------------

  # Convert shapefile into simple features data frame
  # NOTE:  The 'NWSFireZones' directory has been created
  dsnPath <- file.path(dataDir, 'NWSFireZones')
  shpName <- 'fz13se22'
  SFDF <- .convertLayer(
    dsn = dsnPath,
    layer = shpName
  )

  # ----- Select useful columns and rename -------------------------------------

  # > dplyr::glimpse(SFDF, width = 75)
  # Rows: 3,630
  # Columns: 10
  # $ STATE      <chr> "OR", "OR", "OR", "OR", "OR", "OR", "OR", "OR", "OR", …
  # $ ZONE       <chr> "639", "611", "637", "646", "644", "641", "610", "640"…
  # $ CWA        <chr> "PDT", "PDT", "BOI", "BOI", "PDT", "PDT", "PDT", "PDT"…
  # $ NAME       <chr> "East Slopes of the Northern Oregon Cascades", "Deschu…
  # $ STATE_ZONE <chr> "OR639", "OR611", "OR637", "OR646", "OR644", "OR641", …
  # $ TIME_ZONE  <chr> "P", "P", "P", "P", "P", "P", "P", "P", "P", "P", "P",…
  # $ FE_AREA    <chr> "nc", "sw", "se", "ne", "ne", "nc", "nc", "sw", "ne", …
  # $ LON        <dbl> -121.4000, -121.5055, -117.6212, -117.5817, -118.9154,…
  # $ LAT        <dbl> 45.3804, 43.6707, 43.1807, 44.6684, 44.9152, 45.4969, …
  # $ geometry   <MULTIPOLYGON [°]> MULTIPOLYGON (((-121.4827 4..., MULTIPOLY…

  # Data Dictionary:
  #   STATE -------> stateCode: 2-digit postal code
  #   ZONE --------> zoneNumber: zone number
  #   CWA ---------> weatherForecastOffice: WFO responsible for zone
  #   NAME --------> name: name of forecast zone
  #   STATE_ZONE --> zoneID: state code and zone number
  #   TIME_ZONE ---> (drop)
  #   FE_AREA -----> (drop)
  #   LON ---------> longitude: longitude of zone centroid
  #   LAT ---------> latitude: latitude of zone centroid

  # NOTE:  The values in the TIME_ZONE field do not correspond to timezones from the
  # NOTE:  WorldTimezones file so that field will be dropped to avoid confusion.
  # NOTE:  For example:
  # NOTE:  loadSpatialData("WorldTimezones")
  # NOTE:  plot(dplyr::filter(SFDF, TIME_ZONE == "A"))
  # NOTE:  plot(dplyr::filter(WorldTimezones, timezone == "America/Anchorage"), border = "red", add = TRUE)
  # NOTE:  plot(dplyr::filter(WorldTimezones, timezone == "America/Nome"), border = "blue", add = TRUE)

  # Create the new dataframe in a specific column order
  SFDF <-
    dplyr::select(
      .data = SFDF,
      stateCode = .data$STATE,
      weatherForecastOffice = .data$CWA,
      zoneNumber = .data$ZONE,
      name = .data$NAME,
      zoneID = .data$STATE_ZONE,
      longitude = .data$LON,
      latitude = .data$LAT
    )

  # ----- Clean SFDF -----------------------------------------------------------

  uniqueIdentifier <- "zoneID"

  # Guarantee that all polygons are unique
  if ( any(duplicated(SFDF[[uniqueIdentifier]])) )
    stop(sprintf("Column '%s' has multiple records. An organizePolygons() step is needed.", uniqueIdentifier))

  # All polygons are unique so we just add polygonID manually
  SFDF$polygonID <- as.character(seq_len(nrow(SFDF)))

  # Guarantee that all geometries are valid
  if ( any(!sf::st_is_valid(SFDF)) )
    SFDF <- sf::st_make_valid(SFDF)

  # ----- Name and save the data -----------------------------------------------

  # Assign a name and save the data
  message("Saving full resolution version...\n")
  assign(datasetName, SFDF)
  save(list = c(datasetName), file = paste0(dataDir, '/', datasetName, '.rda'))
  rm(list = datasetName)

  # * Simplify -----

  if ( simplify )
    .simplifyAndSave(SFDF, datasetName, dataDir)

  # ----- Clean up and return --------------------------------------------------

  # Clean up
  unlink(filePath, force = TRUE)
  unlink(dsnPath, recursive = TRUE, force = TRUE)

  return(invisible(datasetName))

}
