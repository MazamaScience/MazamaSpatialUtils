#' @importFrom rlang .data
#' @export
#'
#' @title Convert US Census state shapefile
#'
#' @description Create a simple features dataframe for US states
#'
#' The full resolution file will be named "USCensusStates". In addition,
#' "_05", _02" and "_01" versions of the file will be created that that are
#' simplified to 5\%, 2\% and 1\%. Simplified versions will greatly improve the
#' speed of both searching and plotting.
#'
#' @details A US state borders shapefile is downloaded and converted to a
#' simple features dataframe with additional columns of data. The resulting file
#' will be created in the spatial data directory which is set with
#' \link{setSpatialDataDir}.
#'
#' The source data is from 2021.
#'
#' @note From the source documentation:
#'
#' \strong{Cartographic Boundary Files}
#'
#' The cartographic boundary files are simplified representations of
#' selected geographic areas from the U.S. Census Bureau's Master Address File /
#' Topologically Integrated Geographic Encoding and Referencing (MAF/TIGER)
#' Database (MTDB). These boundary files are specifically designed for
#' small-scale thematic mapping. When possible, generalization is performed with
#' the intent to maintain the hierarchical relationships among geographies and
#' to maintain the alignment of geographies within a file set for a given year.
#' To improve the appearance of shapes, areas are represented with fewer vertices
#' than detailed TIGER/Line equivalents. Some small holes or discontiguous parts
#' of areas are not included in generalized files. Generalized boundary files
#' are clipped to a simplified version of the U.S. outline. As a result, some
#' offshore areas may be excluded from the generalized files.
#'
#' \strong{Limitations}
#'
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
#' @return Name of the datasetName being created.
#'
#' @references \url{https://www2.census.gov/geo/tiger/GENZ2021/}
#'

convertUSCensusStates <- function() {

  # ----- Setup ----------------------------------------------------------------

  # Use package internal data directory
  dataDir <- getSpatialDataDir()

  # Specify the name of the dataset and file being created
  datasetName <- 'USCensusStates'

  # ----- Get the data ---------------------------------------------------------

  # Build appropriate request URL
  # NOTE: 500k means resolution level 1:500k.
  url <- 'https://www2.census.gov/geo/tiger/GENZ2021/shp/cb_2021_us_state_500k.zip'

  filePath <- file.path(dataDir, basename(url))
  utils::download.file(url, filePath)
  # NOTE:  This zip file has no directory so extra subdirectory needs to be created
  utils::unzip(filePath, exdir = file.path(dataDir, 'states'))

  # ----- Convert to SFDF ------------------------------------------------------

  # Convert shapefile into simple features data frame
  dsnPath <- file.path(dataDir, 'states')
  shpName <- 'cb_2021_us_state_500k'
  SFDF <- convertLayer(
    dsn = dsnPath,
    layer = shpName
  )

  # ----- Select useful columns and rename -------------------------------------

  # > dplyr::glimpse(SFDF, width = 75)
  # Rows: 56
  # Columns: 10
  # $ STATEFP  <chr> "56", "02", "24", "60", "05", "38", "10", "66", "35", "4…
  # $ STATENS  <chr> "01779807", "01785533", "01714934", "01802701", "0006808…
  # $ AFFGEOID <chr> "0400000US56", "0400000US02", "0400000US24", "0400000US6…
  # $ GEOID    <chr> "56", "02", "24", "60", "05", "38", "10", "66", "35", "4…
  # $ STUSPS   <chr> "WY", "AK", "MD", "AS", "AR", "ND", "DE", "GU", "NM", "U…
  # $ NAME     <chr> "Wyoming", "Alaska", "Maryland", "American Samoa", "Arka…
  # $ LSAD     <chr> "00", "00", "00", "00", "00", "00", "00", "00", "00", "0…
  # $ ALAND    <dbl> 2.514587e+11, 1.478943e+12, 2.515199e+10, 1.977591e+08, …
  # $ AWATER   <dbl> 1867503716, 245378425142, 6979074857, 1307243751, 312195…
  # $ geometry <MULTIPOLYGON [°]> MULTIPOLYGON (((-111.0546 4..., MULTIPOLYGO…

  # Remove outlying territories
  SFDF <-
    SFDF %>%
    dplyr::filter(.data$STUSPS %in% US_52)

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
  SFDF$ALAND <- as.numeric(SFDF$ALAND)
  SFDF$AWATER <- as.numeric(SFDF$AWATER)

  SFDF$countryCode <- "US"

  # Create the new dataframe in a specific column order
  SFDF <-
    SFDF %>%
    dplyr::select(
      countryCode = .data$countryCode,
      stateCode = .data$STUSPS,
      stateFIPS = .data$STATEFP,
      stateName = .data$NAME,
      landArea = .data$ALAND,
      waterArea = .data$AWATER,
      AFFGEOID = .data$AFFGEOID
    )

  # ----- Simplify and save ----------------------------------------------------

  uniqueIdentifier <- "stateFIPS"

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
