#' @keywords datagen
#' @importFrom rlang .data
#' @export
#'
#' @title Convert Indian Lands Shapefile
#'
#' @param nameOnly Logical specifying whether to only return the name without
#' creating the file.
#' @param simplify Logical specifying whether to create "_05", _02" and "_01"
#' versions of the file that are simplified to 5\%, 2\% and 1\%.
#'
#' @description Create a SpatialPolygonsDataFrame for Native American land.
#'
#' @details A Native American land shapefile is downloaded and converted to a
#' SpatialPolygonsDataFrame with additional columns of data. The resulting file
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
#' @return Name of the dataset being created.
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

  # ----- Convert to SPDF ------------------------------------------------------

  # Convert shapefile into SpatialPolygonsDataFrame
  # NOTE:  The 'indlan' directory has been created
  dsnPath <- file.path(dataDir, 'indlan')
  shpName <- 'indlanp010g'
  SPDF <- convertLayer(
    dsn = dsnPath,
    layerName = shpName,
    encoding = 'UTF-8'
  )

  # ----- Select useful columns and rename -------------------------------------

  # > dplyr::glimpse(SPDF@data)
  # Observations: 558
  # Variables: 23
  # $ OBJECTID   <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,…
  # $ AREA       <dbl> 391.0876336, 2.0070783, 66.4574107, 50.9686454, 1.0109839,…
  # $ PERIMETER  <dbl> 133.717749, 9.271149, 205.966316, 37.774333, 4.022588, 19.…
  # $ Indlanp010 <int> 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163…
  # $ FEATURE1   <chr> "Indian Reservation", "Indian Reservation", "Indian Reserv…
  # $ GNIS_Name1 <chr> "Acoma Pueblo", "Acoma Pueblo", "Agua Caliente Indian Rese…
  # $ GNIS_ID1   <chr> "1934337", "1934337", "1934324", "912566", "238830", "2559…
  # $ ADMIN1     <chr> "BIA", "BIA", "BIA", "BIA", "BIA", "BIA", "BIA", "BIA", "B…
  # $ FEATURE2   <chr> "N/A", "Public Domain Land", "N/A", "National Forest", "Na…
  # $ GNIS_Name2 <chr> "N/A", "N/A", "N/A", "Cibola National Forest", "N/A", "N/A…
  # $ GNIS_ID2   <chr> "N/A", "N/A", "N/A", "1851853", "N/A", "N/A", "N/A", "N/A"…
  # $ ADMIN2     <chr> "N/A", "BLM", "N/A", "FS", "BLM", "N/A", "N/A", "BLM", "N/…
  # $ FEATURE3   <chr> "N/A", "N/A", "N/A", "N/A", "N/A", "N/A", "N/A", "N/A", "N…
  # $ GNIS_Name3 <chr> "N/A", "N/A", "N/A", "N/A", "N/A", "N/A", "N/A", "N/A", "N…
  # $ GNIS_ID3   <chr> "N/A", "N/A", "N/A", "N/A", "N/A", "N/A", "N/A", "N/A", "N…
  # $ ADMIN3     <chr> "N/A", "N/A", "N/A", "N/A", "N/A", "N/A", "N/A", "N/A", "N…
  # $ URL        <chr> "N/A", "N/A", "N/A", "N/A", "N/A", "N/A", "N/A", "N/A", "N…
  # $ STATE      <chr> "NM", "NM", "CA", "NM", "CA", "CA", "CA", "CA", "CA", "CA"…
  # $ STATE_FIPS <chr> "35", "35", "06", "35", "06", "06", "06", "06", "06", "06"…
  # $ ORIG_NAME  <chr> "ACOMA PUEBLO", "ACOMA PUEBLO", "AGUA CALIENTE INDIAN RESE…
  # $ GIS_ACRES  <dbl> 250296.0855, 1284.5301, 42532.7428, 32619.9330, 647.0297, …
  # $ SHAPE_Leng <dbl> 2.15188890, 0.15164941, 3.28540939, 0.61393712, 0.06411717…
  # $ SHAPE_Area <dbl> 0.0998716951, 0.0005123971, 0.0167604554, 0.0129362866, 0.…

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
  SPDF@data <- as.data.frame(apply(SPDF@data, 2, nafun), stringsAsFactors = FALSE)

  # Remove rows that are not indian reservations
  SPDF <- SPDF[which(SPDF@data$FEATURE1 == "Indian Reservation"),]

  # Convert area from square miles to m^2
  SPDF@data$AREA <- as.numeric(SPDF$AREA)
  SPDF@data$AREA <- SPDF$AREA*1609.344^2

  # Get latitude and longitude from polygon centroids
  centroids <- rgeos::gCentroid(SPDF, byid = TRUE)
  lon <- sp::coordinates(centroids)[,1]
  lat <- sp::coordinates(centroids)[,2]

  SPDF@data$longitude <- lon
  SPDF@data$latitude <- lat

  # NOTE: There are 21 polygons which span more than one state. We can use longitude and latitude to
  # get one state code for each polygon.
  SPDF@data$stateCode <- getStateCode(
    SPDF@data$longitude,
    SPDF@data$latitude,
    dataset = 'USCensusStates',
    useBuffering = TRUE
  )

  SPDF@data$countryCode <- "US"

  SPDF@data$allStateCodes <- stringr::str_replace_all(SPDF@data$STATE, "-", ",")

  SPDF@data <-
    dplyr::select(
      .data = SPDF@data,
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

  # ----- Clean SPDF -----------------------------------------------------------

  # Group polygons with the same identifier (GNISCode)
  SPDF <- organizePolygons(
    SPDF,
    uniqueID = "GNISCode",
    sumColumns = "area"
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
