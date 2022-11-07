#' @keywords datagen
#' @export
#'
#' @title Convert US congressional districts shapefile
#'
#' @description Returns a simple features data frame for US Congressional Districts
#' for the 116th US House of Representatives.
#'
#' The full resolution file will be named "USCensus116thCongress.rda". In addition,
#' "_05", _02" and "_01" versions of the file will be created that that are
#' simplified to 5\%, 2\% and 1\%. Simplified versions will greatly improve the
#' speed of both searching and plotting.
#'
#' @details A US congressional district shapefile is downloaded and converted to
#' a simple features data frame with additional columns of data. The resulting
#' file will be created in the spatial data directory which is set with
#' \code{setSpatialDataDir()}.
#'
#' #' The source data is from 2021.
#'
#' @note From the source documentation:
#'
#' Congressional Districts are the 435 areas from which people are elected to the
#' U.S. House of Representatives. After the apportionment of congressional seats
#' among the states based on census population counts, each state is responsible
#' for establishing congressional districts for the purpose of electing representatives.
#' Each congressional district is to be as equal in population to all other
#' congressional districts in a state as practicable. The 116th Congress is seated
#' from January 2019 to 2021. The cartographic boundary files for the District of
#' Columbia, Puerto Rico, and the Island Areas (American Samoa, Guam, the Commonwealth
#' of the Northern Mariana Islands, and the U.S. Virgin Islands) each contain a
#' single record for the non-voting delegate district in these areas. The boundaries
#' of all other congressional districts are provided to the Census Bureau by the
#' states by May 1, 2018.
#'
#' You can join this file with table data downloaded from American FactFinder by
#' using the AFFGEOID field in the cartographic boundary file.
#'
#' @return Name of the datasetName being created.
#'
#' @references \url{https://www2.census.gov/geo/tiger/GENZ2021/}
#'
#' @seealso setSpatialDataDir

convertUSCensusCongress <- function() {

  # ----- Setup ----------------------------------------------------------------

  # Use package internal data directory
  dataDir <- getSpatialDataDir()

  # Specify the name of the dataset and file being created
  datasetName <- 'USCensus116thCongress'

  # ----- Get the data ---------------------------------------------------------

  # Build appropriate request URL
  # NOTE: 500k means resolution level 1:500k.
  # RC Note: cd116 means Congressional District (116th Congress)
  url <- 'https://www2.census.gov/geo/tiger/GENZ2021/shp/cb_2021_us_cd116_500k.zip'

  filePath <- file.path(dataDir, basename(url))
  utils::download.file(url, filePath)
  # NOTE:  This zip file has no directory so extra subdirectory needs to be created
  utils::unzip(filePath, exdir = file.path(dataDir, 'congress'))

  # ----- Convert to SFDF ------------------------------------------------------

  # Convert shapefile into simple features data frame
  # NOTE:  The 'congress' directory has been created
  dsnPath <- file.path(dataDir,'congress')
  shpName <- 'cb_2021_us_cd116_500k'
  SFDF <- convertLayer(
    dsn = dsnPath,
    layer = shpName
  )

  # ----- Select useful columns and rename -------------------------------------

  # > dplyr::glimpse(SFDF, width = 75)
  # Rows: 441
  # Columns: 10
  # $ STATEFP  <chr> "06", "22", "35", "41", "53", "06", "36", "01", "09", "2…
  # $ CD116FP  <chr> "15", "04", "03", "02", "03", "31", "06", "01", "04", "0…
  # $ AFFGEOID <chr> "5001600US0615", "5001600US2204", "5001600US3503", "5001…
  # $ GEOID    <chr> "0615", "2204", "3503", "4102", "5303", "0631", "3606", …
  # $ NAMELSAD <chr> "Congressional District 15", "Congressional District 4",…
  # $ LSAD     <chr> "C2", "C2", "C2", "C2", "C2", "C2", "C2", "C2", "C2", "C…
  # $ CDSESSN  <chr> "116", "116", "116", "116", "116", "116", "116", "116", …
  # $ ALAND    <dbl> 1549309489, 32210673963, 116460386484, 179877899384, 236…
  # $ AWATER   <dbl> 73721922, 1104876825, 301515882, 1885211282, 1216260406,…
  # $ geometry <MULTIPOLYGON [°]> MULTIPOLYGON (((-122.1709 3..., MULTIPOLYGO…

  # Data Dictionary:
  #   STATEFP -----> stateFIPS: 2-digit FIPS code
  #   CD116FP -----> congressionalDistrictFIPS
  #   AFFGEOID ----> AFFGEOID
  #   GEOID -------> GeoID
  #   NAMELSAD ----> (drop)
  #   LSAD --------> (drop)
  #   CDSESSN ------> (drop) this is the congressional district session number of the datasetName (116 for all)
  #   ALAND -------> landArea: land area (in sq. meters)
  #   AWATER ------> waterArea: water area (in sq. meters)

  # Guarantee that ALAND and AWATER are numeric
  SFDF$ALAND <- as.numeric(SFDF$ALAND)
  SFDF$AWATER <- as.numeric(SFDF$AWATER)

  SFDF$countryCode <- "US"
  SFDF$stateCode <- US_stateFIPSToCode(SFDF$STATEFP)

  # Remove outlying territories
  SFDF <- dplyr::filter(SFDF, .data$stateCode %in% US_52)

  # Create the new dataframe in a specific column order
  SFDF <-
    SFDF %>%
    dplyr::select(
      countryCode = .data$countryCode,
      stateCode = .data$stateCode,
      stateFIPS = .data$STATEFP,
      congressionalDistrictFIPS = .data$CD116FP,
      landArea = .data$ALAND,
      waterArea = .data$AWATER,
      AFFGEOID = .data$AFFGEOID,
      GeoID = .data$GEOID
    )

  # ----- Simplify and save ----------------------------------------------------

  uniqueIdentifier <- "GeoID"

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











