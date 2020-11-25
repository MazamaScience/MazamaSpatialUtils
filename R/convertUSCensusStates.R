#' @keywords datagen
#' @importFrom rlang .data
#' @export
#'
#' @title Convert US Census State Shapefile
#'
#' @param nameOnly Logical specifying whether to only return the name without
#' creating the file.
#' @param simplify Logical specifying whether to create "_05", _02" and "_01"
#' versions of the file that are simplified to 5\%, 2\% and 1\%.
#'
#' @description Create a SpatialPolygonsDataFrame for US states
#'
#' @details A US state borders shapefile is downloaded and converted to a
#' SpatialPolygonsDataFrame with additional columns of data. The resulting file
#' will be created in the spatial data directory which is set with
#' \code{setSpatialDataDir()}.
#'
#' The source data is from 2019.
#'
#' @note From the source documentation:
#'
#' The 2019 cartographic boundary shapefiles are simplified representations of
#' selected geographic areas from the U.S. Census Bureau's Master Address File /
#' Topologically Integrated Geographic Encoding and Referencing (MAF/TIGER)
#' Database (MTDB). These boundary files are specifically designed for
#' small-scale thematic mapping. When possible, generalization is performed with
#' the intent to maintain the hierarchical relationships among geographies and
#' to maintain the alignment of geographies within a file set for a given year.
#' Geographic areas may not align with the same areas from another year. Some
#' geographies are available as nation-based files while others are available
#' only as state-based files.
#'
#' States and equivalent entities are the primary governmental divisions of the
#' United States. In addition to the fifty states, the Census Bureau treats the
#' District of Columbia, Puerto Rico, and each of the Island Areas (American
#' Samoa, the Commonwealth of the Northern Mariana Islands, Guam, and the U.S.
#' Virgin Islands) as the statistical equivalents of states for the purpose of
#' data presentation.
#'
#' \strong{\emph{"Island Areas" are removed in the MazamaSpatialUtils version.}}
#'
#' These files were specifically created to support small-scale thematic mapping.
#' To improve the appearance of shapes at small scales, areas are represented
#' with fewer vertices than detailed TIGER/Line Shapefiles. Cartographic boundary
#' files take up less disk space than their ungeneralized counterparts.
#' Cartographic boundary files take less time to render on screen than TIGER/Line
#' Shapefiles. You can join this file with table data downloaded from American
#' FactFinder by using the AFFGEOID field in the cartographic boundary file. If
#' detailed boundaries are required, please use the TIGER/Line Shapefiles instead
#' of the generalized cartographic boundary files.
#'
#' @return Name of the dataset being created.
#'
#' @references \url{https://www2.census.gov/geo/tiger/GENZ2019/}
#'
#' @seealso setSpatialDataDir
#' @seealso getState
#' @seealso getCode
#' @seealso getName

convertUSCensusStates <- function(
  nameOnly = FALSE,
  simplify = TRUE
) {

  # ----- Setup ----------------------------------------------------------------

  # Use package internal data directory
  dataDir <- getSpatialDataDir()

  # Specify the name of the dataset and file being created
  datasetName <- 'USCensusStates'

  if (nameOnly)
    return(datasetName)

  # ----- Get the data ---------------------------------------------------------

  # Build appropriate request URL
  # NOTE: 500k means resolution level 1:500k.
  url <- 'https://www2.census.gov/geo/tiger/GENZ2019/shp/cb_2019_us_state_500k.zip'

  filePath <- file.path(dataDir, basename(url))
  utils::download.file(url, filePath)
  # NOTE:  This zip file has no directory so extra subdirectory needs to be created
  utils::unzip(filePath, exdir = file.path(dataDir, 'states'))

  # ----- Convert to SPDF ------------------------------------------------------

  # Convert shapefile into SpatialPolygonsDataFrame
  # NOTE:  The 'states' directory has been created
  dsnPath <- file.path(dataDir, 'states')
  shpName <- 'cb_2019_us_state_500k'
  SPDF <- convertLayer(
    dsn = dsnPath,
    layerName = shpName,
    encoding = 'UTF-8'
  )

  # ----- Select useful columns and rename -------------------------------------

  # > dplyr::glimpse(SPDF@data)
  # Observations: 56
  # Variables: 9
  # $ STATEFP  <chr> "12", "78", "30", "27", "24", "45", "23", "15", "11", "69", "4…
  # $ STATENS  <chr> "00294478", "01802710", "00767982", "00662849", "01714934", "0…
  # $ AFFGEOID <chr> "0400000US12", "0400000US78", "0400000US30", "0400000US27", "0…
  # $ GEOID    <chr> "12", "78", "30", "27", "24", "45", "23", "15", "11", "69", "4…
  # $ STUSPS   <chr> "FL", "VI", "MT", "MN", "MD", "SC", "ME", "HI", "DC", "MP", "R…
  # $ NAME     <chr> "Florida", "United States Virgin Islands", "Montana", "Minneso…
  # $ LSAD     <chr> "00", "00", "00", "00", "00", "00", "00", "00", "00", "00", "0…
  # $ ALAND    <chr> "138947364717", "348021896", "376966832749", "206230065476", "…
  # $ AWATER   <chr> "31362872853", "1550236199", "3869031338", "18942261495", "697…

  # Remove outlying territories
  SPDF <- subset(SPDF, SPDF@data$STUSPS %in% US_52)

  # Data Dictionary:
  #   STATEFP -----> stateFIPS: 2-digit FIPS code
  #   STATENS -----> (drop)
  #   AFFGEOID ----> AFFGEOID
  #   GEOID -------> (drop)
  #   STUSPS ------> stateCode: 2-character postal code
  #   NAME --------> stateName: English language name
  #   LSAD --------> (drop)
  #   ALAND -------> landArea: land area (in sq. meters)
  #   AWATER ------> waterArea: water area (in sq. meters)

  # Guarantee that ALAND and AWATER are numeric
  SPDF@data$ALAND <- as.numeric(SPDF@data$ALAND)
  SPDF@data$AWATER <- as.numeric(SPDF@data$AWATER)

  SPDF@data$countryCode <- "US"

  # Create the new dataframe in a specific column order
  SPDF@data <-
    dplyr::select(
      .data = SPDF@data,
      countryCode = .data$countryCode,
      stateCode = .data$STUSPS,
      stateFIPS = .data$STATEFP,
      stateName = .data$NAME,
      landArea = .data$ALAND,
      waterArea = .data$AWATER,
      AFFGEOID = .data$AFFGEOID
    )

  # ----- Clean SPDF -----------------------------------------------------------

  # Group polygons with the same identifier (stateFIPS)
  SPDF <- organizePolygons(
    SPDF,
    uniqueID = 'stateFIPS',
    sumColumns = c('landArea', 'waterArea')
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

