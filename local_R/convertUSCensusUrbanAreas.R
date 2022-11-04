#' @keywords datagen
#' @importFrom rlang .data
#' @export
#'
#' @title Convert US Census Urban Areas shapefiles
#'
#' @param nameOnly Logical specifying whether to only return the name without
#' creating the file.
#' @param simplify Logical specifying whether to create "_05", _02" and "_01"
#' versions of the file that are simplified to 5\%, 2\% and 1\%.
#'
#' @description Create a simple features data frame for US states
#'
#' @details A US county borders shapefile is downloaded and converted to a
#' simple features data frame with additional columns of data. The resulting file
#' will be created in the spatial data directory which is set with
#' \code{setSpatialDataDir()}.
#'
#' The source data is from 2019.
#'
#' @note From the source documentation:
#'
#' After each decennial census, the Census Bureau delineates urban areas that
#' represent densely developed territory, encompassing residential, commercial,
#' and other nonresidential urban land uses.  In general, this territory consists
#' of areas of high population density and urban land use resulting in a
#' representation of the "urban footprint."  There are two types of urban areas:
#' urbanized areas (UAs) that contain 50,000 or more people and urban clusters (UCs)
#' that contain at least 2,500 people, but fewer than 50,000 people (except in
#' the U.S. Virgin Islands and Guam which each contain urban clusters with
#' populations greater than 50,000).  Each urban area is identified by a 5-character
#' numeric census code that may contain leading zeroes.
#'
#' @return Name of the datasetName being created.
#'
#' @references \url{https://www2.census.gov/geo/tiger/TIGER2019/UAC/}

convertUSCensusUrbanAreas <- function(
  nameOnly = FALSE,
  simplify = TRUE
) {

  # ----- Setup ----------------------------------------------------------------

  # Use package internal data directory
  dataDir <- getSpatialDataDir()

  # Specify the name of the dataset and file being created
  datasetName <- "USCensusUrbanAreas"

  if (nameOnly)
    return(datasetName)

  # ----- Get the data ---------------------------------------------------------

  # Build appropriate request URL
  url <- 'https://www2.census.gov/geo/tiger/TIGER2019/UAC/tl_2019_us_uac10.zip'

  filePath <- file.path(dataDir, basename(url))
  utils::download.file(url, filePath)
  # NOTE:  This zip file has no directory so extra subdirectory needs to be created
  utils::unzip(filePath, exdir = file.path(dataDir, 'us_census_urban_areas'))

  # ----- Convert to SFDF ------------------------------------------------------

  # Convert shapefile into simple features data frame
  dsnPath <- file.path(dataDir, 'us_census_urban_areas')
  shpName <- 'tl_2019_us_uac10'
  SFDF <- .convertLayer(
    dsn = dsnPath,
    layer = shpName,
    encoding = 'UTF-8'
  )

  # ----- Select useful columns and rename -------------------------------------

  message("Harmonizing @data...\n")

  #   > dplyr::glimpse(SFDF)
  #   Observations: 3,601
  #   Variables: 12
  #   $ UACE10     <chr> "24310", "27847", "18100", "06166", "75270...
  #   $ GEOID10    <chr> "24310", "27847", "18100", "06166", "75270...
  #   $ NAME10     <chr> "Dixon, IL", "Escanaba, MI", "Clintonville...
  #   $ NAMELSAD10 <chr> "Dixon, IL Urban Cluster", "Escanaba, MI U...
  #   $ LSAD10     <chr> "76", "76", "76", "76", "76", "75", "76", ...
  #   $ MTFCC10    <chr> "G3500", "G3500", "G3500", "G3500", "G3500...
  #   $ UATYP10    <chr> "C", "C", "C", "C", "C", "U", "C", "C", "U...
  #   $ FUNCSTAT10 <chr> "S", "S", "S", "S", "S", "S", "S", "S", "S...
  #   $ ALAND10    <chr> "25525003", "46648248", "5854683", "304025...
  #   $ AWATER10   <chr> "938058", "283456", "502563", "2314", "0",...
  #   $ INTPTLAT10 <chr> "+41.8529507", "+45.8704839", "+44.6232203...
  #   $ INTPTLON10 <chr> "-089.4817439", "-087.0638396", "-088.7611...

  # Data Dictionary:
  #   UACE10 -----> (drop)
  #   GEOID10 ----> GEOID: Urban area identifier
  #   NAME10 -----> urbanAreaName: 2010 Census urban area name
  #   NAMELSAD10 -> (drop)
  #   LSAD10 -----> (drop)
  #   MTFCC10 ----> (drop)
  #   UATYP10 ----> urbanAreaType: "U" = urbanized area, "C" = urban cluster
  #   FUNCSTAT10 -> (drop)
  #   ALAND10 ----> landArea: land area (in sq. meters)
  #   AWATER10 ---> waterArea: water area (in sq. meters)
  #   INTPTLAT10 -> latitude
  #   INTPTLON10 -> longitude

  # Convert character fields to numeric and double as needed
  SFDF$ALAND10 <- as.numeric(SFDF$ALAND10)
  SFDF$AWATER10 <- as.numeric(SFDF$AWATER10)

  SFDF$INTPTLAT10 <- as.numeric(SFDF$INTPTLAT10)
  SFDF$INTPTLON10 <- as.numeric(SFDF$INTPTLON10)

  # Convert UATYP10 values into human-readable forms
  is_urban <- SFDF$UATYP10 == "U"
  is_cluster <-  SFDF$UATYP10 == "C"

  SFDF$UATYP10[is_urban] <- "Urbanized Area"
  SFDF$UATYP10[is_cluster] <- "Urban Cluster"

  # Get allStateCodes from the NAME10 column
  nameMatrix <- stringr::str_split_fixed(SFDF$NAME10, ',', 2)
  SFDF$allStateCodes <- stringr::str_trim(stringr::str_replace_all(nameMatrix[,2], '--', ','))

  # We can use longitude and latitude to get one state code for each polygon.
  loadSpatialData("USCensusStates")
  SFDF$stateCode <- getStateCode(SFDF$INTPTLON10, SFDF$INTPTLAT10, datasetName = 'USCensusStates', useBuffering = TRUE)
  SFDF$countryCode <- "US"

  # Remove outlying territories
  SFDF <- subset(SFDF, SFDF$stateCode %in% US_52)

  # Create the new dataframe in a specific column order
  SFDF <- dplyr::select(
    SFDF,
    countryCode = .data$countryCode,
    stateCode = .data$stateCode,
    allStateCodes = .data$allStateCodes,
    urbanAreaName = .data$NAME10,
    urbanAreaType = .data$UATYP10,
    landArea = .data$ALAND10,
    waterArea = .data$AWATER10,
    longitude = .data$INTPTLON10,
    latitude = .data$INTPTLAT10,
    GEOID = .data$GEOID10
  )

  # ----- Organize polygons ----------------------------------------------------

  # Group polygons with the same identifier (GEOID)
  message("Organizing polygons...\n")
  SFDF <- organizePolygons(
    SFDF,
    uniqueID = "GEOID",
    sumColumns = c('landArea', 'waterArea')
  )

  # Clean topology errors
  message("Checking for topology errors...\n")
  if ( !cleangeo::clgeo_IsValid(SFDF) ) {
    message("Cleaning topology errors...\n")
    SFDF <- cleangeo::clgeo_Clean(SFDF)
  }

  # ----- Name and save the data -----------------------------------------------

  # Assign a name and save the data
  message("Saving full resolution version...\n")
  assign(datasetName, SFDF)
  save(list = c(datasetName), file = paste0(dataDir, '/', datasetName, '.rda'))
  rm(list = datasetName)

  # ----- Simplify -------------------------------------------------------------

  if ( simplify ) {
    # Create new, simplified datsets: one with 5%, 2%, and one with 1% of the vertices of the original
    # NOTE:  This may take several minutes.
    message("Simplifying to 5%...\n")
    SFDF_05 <- rmapshaper::ms_simplify(SFDF, 0.05)
    SFDF_05@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
    # Clean topology errors
    if ( !cleangeo::clgeo_IsValid(SFDF_05) ) {
      SFDF_05 <- cleangeo::clgeo_Clean(SFDF_05)
    }
    datasetName_05 <- paste0(datasetName, "_05")
    message("Saving 5% version...\n")
    assign(datasetName_05, SFDF_05)
    save(list = datasetName_05, file = paste0(dataDir,"/", datasetName_05, '.rda'))
    rm(list = c("SFDF_05",datasetName_05))

    message("Simplifying to 2%...\n")
    SFDF_02 <- rmapshaper::ms_simplify(SFDF, 0.02)
    SFDF_02@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
    # Clean topology errors
    if ( !cleangeo::clgeo_IsValid(SFDF_02) ) {
      SFDF_02 <- cleangeo::clgeo_Clean(SFDF_02)
    }
    datasetName_02 <- paste0(datasetName, "_02")
    message("Saving 2% version...\n")
    assign(datasetName_02, SFDF_02)
    save(list = datasetName_02, file = paste0(dataDir,"/", datasetName_02, '.rda'))
    rm(list = c("SFDF_02",datasetName_02))

    message("Simplifying to 1%...\n")
    SFDF_01 <- rmapshaper::ms_simplify(SFDF, 0.01)
    SFDF_01@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
    # Clean topology errors
    if ( !cleangeo::clgeo_IsValid(SFDF_01) ) {
      SFDF_01 <- cleangeo::clgeo_Clean(SFDF_01)
    }
    datasetName_01 <- paste0(datasetName, "_01")
    message("Saving 1% version...\n")
    assign(datasetName_01, SFDF_01)
    save(list = datasetName_01, file = paste0(dataDir,"/", datasetName_01, '.rda'))
    rm(list = c("SFDF_01",datasetName_01))
  }

  # ----- Clean up and return --------------------------------------------------

  unlink(filePath, force = TRUE)
  unlink(dsnPath, recursive = TRUE, force = TRUE)

  return(invisible(datasetName))

}

