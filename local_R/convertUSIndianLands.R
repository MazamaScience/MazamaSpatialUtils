#' @keywords datagen
#' @importFrom rlang .data
#' @export
#'
#' @title Convert Indian lands shapefile
#'
#' @param nameOnly Logical specifying whether to only return the name without
#' creating the file.
#' @param simplify Logical specifying whether to create "_05", _02" and "_01"
#' versions of the file that are simplified to 5\%, 2\% and 1\%.
#'
#' @description Create a simple features data frame for Native American land.
#'
#' @details A Native American land shapefile is downloaded and converted to a
#' simple features data frame with additional columns of data. The resulting file
#' will be created in the spatial data directory which is set with
#' \code{setSpatialDataDir()}.
#'
#' The source data is from 2014.
#'
#' @note From the source documentation:
#'
#'  This map layer shows Indian lands of the United States. For the most part,
#'  only areas of 320 acres or more are included; some smaller areas deemed to
#'  be important or significant are also included. Federally-administered lands
#'  within a reservation are included for continuity; these may or may not be
#'  considered part of the reservation and are simply described with their
#'  feature type and the administrating Federal agency. Some established Indian
#'  lands which are larger than 320 acres are not included in this map layer
#'  because their boundaries were not available from the owning or administering
#'  agency.The USIndianLands shapefile represents lands administered by the
#'  Bureau of Indian Affairs, ie. Indian reservations
#'
#'  These data are intended for geographic display and analysis at the national
#'  level, and for large regional areas. The data should be displayed and
#'  analyzed at scales appropriate for 1:1,000,000-scale data. No responsibility
#'  is assumed by the National Atlas of the United States in the use of these data.
#'  and is compiled by the National Atlas of the United States of America.
#'
#' @return Name of the datasetName being created.
#'
#' @references \url{https://www.sciencebase.gov/catalog/item/5d150464e4b0941bde5b7658}
#'
#' @seealso setSpatialDataDir
#' @seealso getVariable

convertUSIndianLands <- function(
  nameOnly = FALSE,
  simplify = TRUE
) {

  # ----- Setup ----------------------------------------------------------------

  loadSpatialData("USCensusStates")

  # Use package internal data directory
  dataDir <- getSpatialDataDir()

  # Specify the name of the file being created
  datasetName <- 'USIndianLands'

  if (nameOnly)
    return(datasetName)

  # ----- Get the data ---------------------------------------------------------

  # Build appropriate request URL
  url <- "https://prd-tnm.s3.amazonaws.com/StagedProducts/Small-scale/data/Boundaries/indlanp010g.shp_nt00968.tar.gz"

  filePath <- file.path(dataDir,basename(url))
  utils::download.file(url, filePath)
  # NOTE:  This tar.gz file has no directory so extra subdirectory needs to be created
  utils::untar(filePath, exdir = file.path(dataDir, 'indlan'))

  # ----- Convert to SFDF ------------------------------------------------------

  # Convert shapefile into simple features data frame
  # NOTE:  The 'indlan' directory has been created
  dsnPath <- file.path(dataDir, 'indlan')
  shpName <- 'indlanp010g'
  SFDF <- convertLayer(
    dsn = dsnPath,
    layer = shpName
  )

  # ----- Select useful columns and rename -------------------------------------

  # > dplyr::glimpse(SFDF, width = 75)
  # Rows: 558
  # Columns: 24
  # $ OBJECTID   <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,…
  # $ AREA       <dbl> 391.0876336, 2.0070783, 66.4574107, 50.9686454, 1.0109…
  # $ PERIMETER  <dbl> 133.717749, 9.271149, 205.966316, 37.774333, 4.022588,…
  # $ Indlanp010 <int> 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162,…
  # $ FEATURE1   <chr> "Indian Reservation", "Indian Reservation", "Indian Re…
  # $ GNIS_Name1 <chr> "Acoma Pueblo", "Acoma Pueblo", "Agua Caliente Indian …
  # $ GNIS_ID1   <chr> "1934337", "1934337", "1934324", "912566", "238830", "…
  # $ ADMIN1     <chr> "BIA", "BIA", "BIA", "BIA", "BIA", "BIA", "BIA", "BIA"…
  # $ FEATURE2   <chr> "N/A", "Public Domain Land", "N/A", "National Forest",…
  # $ GNIS_Name2 <chr> "N/A", "N/A", "N/A", "Cibola National Forest", "N/A", …
  # $ GNIS_ID2   <chr> "N/A", "N/A", "N/A", "1851853", "N/A", "N/A", "N/A", "…
  # $ ADMIN2     <chr> "N/A", "BLM", "N/A", "FS", "BLM", "N/A", "N/A", "BLM",…
  # $ FEATURE3   <chr> "N/A", "N/A", "N/A", "N/A", "N/A", "N/A", "N/A", "N/A"…
  # $ GNIS_Name3 <chr> "N/A", "N/A", "N/A", "N/A", "N/A", "N/A", "N/A", "N/A"…
  # $ GNIS_ID3   <chr> "N/A", "N/A", "N/A", "N/A", "N/A", "N/A", "N/A", "N/A"…
  # $ ADMIN3     <chr> "N/A", "N/A", "N/A", "N/A", "N/A", "N/A", "N/A", "N/A"…
  # $ URL        <chr> "N/A", "N/A", "N/A", "N/A", "N/A", "N/A", "N/A", "N/A"…
  # $ STATE      <chr> "NM", "NM", "CA", "NM", "CA", "CA", "CA", "CA", "CA", …
  # $ STATE_FIPS <chr> "35", "35", "06", "35", "06", "06", "06", "06", "06", …
  # $ ORIG_NAME  <chr> "ACOMA PUEBLO", "ACOMA PUEBLO", "AGUA CALIENTE INDIAN …
  # $ GIS_ACRES  <dbl> 250296.0855, 1284.5301, 42532.7428, 32619.9330, 647.02…
  # $ SHAPE_Leng <dbl> 2.15188890, 0.15164941, 3.28540939, 0.61393712, 0.0641…
  # $ SHAPE_Area <dbl> 0.0998716951, 0.0005123971, 0.0167604554, 0.0129362866…
  # $ geometry   <MULTIPOLYGON [°]> MULTIPOLYGON (((-107.5451 3..., MULTIPOLY…

  # Data Dictionary:
  #   OBJECTID ----> (drop)
  #   AREA --------> area: land area (in sq. miles)
  #   PERIMETER ---> (drop)
  #   Indlanp010 --> (drop)
  #   FEATURE1 ----> featureType: type of land
  #   GNIS_Name1 --> GNISname: name of feature
  #   GNIS_ID1 ----> GNIScode: unique identifier of feature
  #   ADMIN1 ------> (drop)
  #   FEATURE2 ----> (drop)
  #   GNIS_Name2 --> (drop)
  #   GNIS_ID2 ----> (drop)
  #   ADMIN2 ------> (drop)
  #   FEATURE3 ----> (drop)
  #   GNIS_Name3 --> (drop)
  #   GNIS_ID3 ----> (drop)
  #   ADMIN3 ------> (drop)
  #   URL ---------> (drop)
  #   STATE -------> allStateCodes: 2-character abbreviation of state names
  #   STATE_FIPS --> stateFIPS: 2-digit FIPS code
  #   ORIG_NAME ---> ORIG_NAME: original name of feature
  #   GIS_ACRES ---> (drop)
  #   SHAPE_Leng --> (drop)
  #   SHAPE_Area --> (drop)

  # Change "N/A" to NA
  nafun <- function(x) {
    ifelse(x == "N/A", NA, x)
  }
  # Force removal of 'geometry' column
  dataBrick <- SFDF
  dataBrick$geometry <- NULL
  colCount <- ncol(dataBrick)
  SFDF[1:colCount] <- as.data.frame(apply(dataBrick, 2, nafun), stringsAsFactors = FALSE)

  # Only keep records that are Indian reservations
  SFDF <- SFDF[which(SFDF$FEATURE1 == "Indian Reservation"),]

  # Convert area from square miles to m^2
  SFDF$AREA <- as.numeric(SFDF$AREA)
  SFDF$AREA <- SFDF$AREA*1609.344^2

  # NOTE:  sf::st_centroid complains of an invalid geometry so we fix that here

  # Guarantee that all geometries are valid
  if ( any(!sf::st_is_valid(SFDF)) )
    SFDF <- sf::st_make_valid(SFDF)

  # Get latitude and longitude from polygon centroids
  centroids <- sf::st_centroid(SFDF)
  lon <- sf::st_coordinates(centroids)[,1]
  lat <- sf::st_coordinates(centroids)[,2]

  SFDF$longitude <- lon
  SFDF$latitude <- lat

  # NOTE: There are 21 polygons which span more than one state. We can use longitude and latitude to
  # get one state code for each polygon.
  SFDF$stateCode <- getStateCode(
    SFDF$longitude,
    SFDF$latitude,
    datasetName = 'USCensusStates',
    useBuffering = TRUE
  )

  SFDF$countryCode <- "US"

  SFDF$allStateCodes <- stringr::str_replace_all(SFDF$STATE, "-", ",")

  SFDF <-
    dplyr::select(
      .data = SFDF,
      countryCode = .data$countryCode,
      stateCode = .data$stateCode,
      allStateCodes = .data$allStateCodes,
      featureType = .data$FEATURE1,
      ORIG_NAME = .data$ORIG_NAME,
      GNISName = .data$GNIS_Name1,
      GNISCode = .data$GNIS_ID1,
      area = .data$AREA,
      longitude = .data$longitude,
      latitude = .data$latitude
    )

  # ----- Clean SFDF -----------------------------------------------------------

  uniqueIdentifier <- "GNISCode"

  # Guarantee that all polygons are unique
  if ( any(duplicated(SFDF[[uniqueIdentifier]])) )
    stop(sprintf("Column '%s' has multiple records. An organizePolygons() step is needed.", uniqueIdentifier))

  # TODO:  Need to organize polygons

  # All polygons are unique so we just add polygonID manually
  SFDF$polygonID <- as.character(seq_len(nrow(SFDF)))

  # NOTE:  Already did this above
  # # Guarantee that all geometries are valid
  # if ( any(!sf::st_is_valid(SFDF)) )
  #   SFDF <- sf::st_make_valid(SFDF)

  # ----- Name and save the data -----------------------------------------------

  # Assign a name and save the data
  message("Saving full resolution version...\n")
  assign(datasetName, SFDF)
  save(list = c(datasetName), file = paste0(dataDir, '/', datasetName, '.rda'))
  rm(list = datasetName)

  # * Simplify -----

  if ( simplify )
    simplifyAndSave(SFDF, datasetName, dataDir)

  # ----- Clean up and return --------------------------------------------------

  # Clean up
  unlink(filePath, force = TRUE)
  unlink(dsnPath, recursive = TRUE, force = TRUE)

  return(invisible(datasetName))

}
