#' @keywords datagen
#' @importFrom rlang .data
#' @export
#'
#' @title Convert Terrestrial Ecoregion Shapefile
#'
#' @param nameOnly Logical specifying whether to only return the name without
#' creating the file.
#' @param simplify Logical specifying whether to create "_05", _02" and "_01"
#' versions of the file that are simplified to 5\%, 2\% and 1\%.
#'
#' @description Create a SpacialPolygonsDataFrame for Terrestrial Ecoregions.
#'
#' @details A Terrestrial Ecoregions shapefile is downloaded and converted to a
#' SpatialPolygonsDataFrame with additional columns of data. The resulting file
#' will be created in the spatial data directory which is set with
#' \code{setSpatialDataDir()}.
#'
#' @note From the source documentation:
#'
#' This map depicts the 825 terrestrial ecoregions of the globe. Ecoregions are
#' relatively large units of land containing distinct assemblages of natural
#' communities and species, with boundaries that approximate the original extent
#' of natural communities prior to major land-use change. This comprehensive,
#' global map provides a useful framework for conducting biogeographical or
#' macroecological research, for identifying areas of outstanding biodiversity
#' and conservation priority, for assessing the representation and gaps in
#' conservation efforts worldwide, and for communicating the global distribution
#' of natural communities on earth. We have based ecoregion delineations on
#' hundreds of previous biogeographical studies, and refined and synthesized
#' existing information in regional workshops over 10 years to assemble the
#' global dataset. Ecoregions are nested within two higher-order
#' classifications: biomes (14) and biogeographic realms (8). Together, these
#' nested classification levels provide a framework for comparison among units
#' and the identification of representative habitats and species assemblages.
#'
#' @return Name of the dataset being created.
#'
#' @references \url{https://www.worldwildlife.org/publications/terrestrial-ecoregions-of-the-world}
#'
#' @seealso setSpatialDataDir
#' @seealso getVariable

convertTerrestrialEcoregions <- function(
  nameOnly = FALSE,
  simplify = TRUE
) {

  # ----- Setup ----------------------------------------------------------------

  # Use package internal data directory
  dataDir <- getSpatialDataDir()

  # Specify the name of the dataset and file being created
  datasetName <- 'TerrestrialEcoregions'

  if (nameOnly)
    return(datasetName)

  # ----- Get the data ---------------------------------------------------------

  # Build appropriate request URL
  url <- "https://c402277.ssl.cf1.rackcdn.com/publications/15/files/original/official_teow.zip?1349272619"

  filePath <- file.path(dataDir, basename(url))
  utils::download.file(url, filePath)
  # NOTE: This zip file has no directory so extra subdirectory needs to be created
  utils::unzip(filePath, exdir = file.path(dataDir, 'terrestrialEcors'))

  # ----- Convert to SPDF ------------------------------------------------------

  # Convert shapefile into SpatialPolygonsDataFrame
  # NOTE: The 'terrestrialEcors' directory has been created
  dsnPath <- file.path(dataDir, 'terrestrialEcors/official')
  shpName <- 'wwf_terr_ecos'
  SPDF <- convertLayer(
    dsn = dsnPath,
    layerName = shpName,
    encoding = 'UTF-8'
  )

  # ----- Select useful columns and rename -------------------------------------

  message("Harmonizing @data...\n")

  # > dplyr::glimpse(SPDF@data)
  # Observations: 14,458
  # Variables: 21
  # $ OBJECTID   <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,…
  # $ AREA       <dbl> 29.802942, 11.977824, 77301.372760, 215.489182, 19.639089,…
  # $ PERIMETER  <dbl> 0.219, 0.193, 111.954, 1.274, 0.363, 14.323, 0.457, 0.268,…
  # $ ECO_NAME   <chr> "Northern Mesoamerican Pacific mangroves", "Northern Mesoa…
  # $ REALM      <chr> "NT", "NT", "NT", "NT", "NT", "NT", "NT", "NT", "NT", "NT"…
  # $ BIOME      <dbl> 14, 14, 2, 14, 14, 9, 14, 14, 3, 14, 3, 3, 14, 14, 14, 14,…
  # $ ECO_NUM    <dbl> 4, 4, 28, 4, 4, 4, 4, 4, 1, 4, 1, 1, 2, 2, 4, 4, 1, 4, 2, …
  # $ ECO_ID     <dbl> 61404, 61404, 60228, 61404, 61404, 60904, 61404, 61404, 60…
  # $ ECO_SYM    <dbl> 119, 119, 98, 119, 119, 59, 119, 119, 85, 119, 85, 85, 119…
  # $ GBL_STAT   <dbl> 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 3, 3, 1, 1, 1, 2, 3, 1…
  # $ G200_REGIO <chr> NA, NA, "Southern Mexican Dry Forests", NA, NA, "Everglade…
  # $ G200_NUM   <dbl> 0, 0, 56, 0, 0, 100, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 100,…
  # $ G200_BIOME <dbl> 0, 0, 2, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0…
  # $ G200_STAT  <dbl> 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
  # $ Shape_Leng <dbl> 0.2194754, 0.1932891, 111.9514139, 1.2736542, 0.3630683, 1…
  # $ Shape_Area <dbl> 0.0027685646, 0.0011112449, 6.8791878265, 0.0197014674, 0.…
  # $ area_km2   <int> 8174, 8174, 77362, 8174, 8174, 20028, 8174, 8174, 6870, 81…
  # $ eco_code   <chr> "NT1404", "NT1404", "NT0228", "NT1404", "NT1404", "NT0904"…
  # $ PER_area   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
  # $ PER_area_1 <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
  # $ PER_area_2 <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…

  # Data Dictionary:
  #   OBJECTID -----> (drop)
  #   AREA ---------> area: the area of the ecoregion in m^2
  #   PERIMETER ----> (drop)
  #   ECO_NAME -----> ecoregionName: name of the ecoregion
  #   REALM --------> realm: biogeographical realm name
  #   BIOME --------> biome: biome name
  #   ECO_NUM ------> ecoregionNumber: unique identifier
  #   ECO_ID -------> ecoregionID: unique identifier
  #   ECO_SYM ------> ECO_SYM: ecoregion symbol
  #   GBL_STAT -----> GBL_STAT: prediction of future status
  #   G200_REGIO ---> G200Region: global 200 region name
  #   G200_NUM -----> G200Number: global 200 unique identifier
  #   G200_BIOME ---> G200Biome: global 200 biome name
  #   G200_STAT ----> G200Stat: global 200 status
  #   Shape_Leng ---> (drop)
  #   Shape_Area ---> (drop)
  #   area_km2 -----> (drop)
  #   eco_code -----> ecoregionCode: 2-character identifier
  #   PER_area -----> (drop)
  #   PER_area_1 ---> (drop)
  #   PER_area_2 ---> (drop)

  # convert area to m^2
  SPDF@data$AREA <- as.numeric(SPDF$AREA)
  SPDF@data$AREA <- SPDF$AREA*1000000

  # Get latitude and longitude from polygon centroids
  centroids <- rgeos::gCentroid(SPDF, byid = TRUE)
  lon <- sp::coordinates(centroids)[,1]
  lat <- sp::coordinates(centroids)[,2]

  SPDF@data$longitude <- lon
  SPDF@data$latitude <- lat

  # Get countryCode from latitude and longitude
  # NOTE: Some records may have countryCode with value NA
  SPDF@data$countryCode <- getCountryCode(SPDF$longitude, SPDF$latitude, useBuffering = FALSE)

  # Create the new dataframe in a specific column order
  # NOTE: ecoregionName, ecoregionID, and ecoregionCode are all equivalent identifiers for each ecoregion
  SPDF@data <-
    dplyr::select(
      .data = SPDF@data,
      ecoregionName = .data$ECO_NAME,
      ecoregionID = .data$ECO_ID,
      ecoregionNumber = .data$ECO_NUM,
      ecoregionCode = .data$eco_code,
      countryCode = .data$countryCode,
      ECO_SYM = .data$ECO_SYM,
      GBL_STAT = .data$GBL_STAT,
      realm = .data$REALM,
      biome = .data$BIOME,
      area = .data$AREA,
      latitude = .data$latitude,
      longitude = .data$longitude,
      G200Region = .data$G200_REGIO,
      G200Number = .data$G200_NUM,
      G200Biome = .data$G200_BIOME,
      G200STAT = .data$G200_STAT
    )

  # ----- Clean SPDF -----------------------------------------------------------

  # Group polygons with the same identifier (stateFIPS)
  message("Organizing polygons...\n")
  SPDF <- organizePolygons(
    SPDF,
    uniqueID = 'ecoregionCode',
    sumColumns = 'area'
  )

  # Clean topology errors
  message("Checking for topology errors...\n")
  if ( !cleangeo::clgeo_IsValid(SPDF) ) {
    message("Cleaning topology errors...\n")
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
    # NOTE: This may take several minutes.
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
