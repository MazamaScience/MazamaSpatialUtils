#' @importFrom rlang .data
#' @export
#'
#' @title Convert US Core Based Statistical Areas shapefile
#'
#' @description Returns a simple features data frame for US CBSAs
#'
#' The full resolution file will be named "USCensusCBSA.rda". In addition,
#' "_05", _02" and "_01" versions of the file will be created that that are
#' simplified to 5\%, 2\% and 1\%. Simplified versions will greatly improve the
#' speed of both searching and plotting.
#'
#' @details A US Core Based Statistical Areas (CBSA) shapefile is downloaded and converted to a
#' simple features data frame with additional columns of data. The resulting file
#' will be created in the spatial data directory which is set with
#' \code{setSpatialDataDir()}.
#'
#' @note From the source documentation:
#'
#' Metropolitan and Micropolitan Statistical Areas are together termed Core Based
#' Statistical Areas (CBSAs) and are defined by the Office of Management and Budget
#' (OMB) and consist of the county or counties or equivalent entities associated
#' with at least one urban core (urbanized area or urban cluster) of at least 10,000
#' population, plus adjacent counties having a high degree of social and economic
#' integration with the core as measured through commuting ties with the counties
#' containing the core. Categories of CBSAs are: Metropolitan Statistical Areas,
#' based on urbanized areas of 50,000 or more population; and Micropolitan Statistical
#' Areas, based on urban clusters of at least 10,000 population but less than 50,000
#' population.
#'
#' The CBSA boundaries are those defined by OMB based on the 2010 Census,
#' published in 2013, and updated in 2020.
#'
#' @return Name of the datasetName being created.
#'
#' @references \url{https://www2.census.gov/geo/tiger/TIGER2021/CBSA/}
#'
#' @seealso setSpatialDataDir
#' @seealso getUSCounty

convertUSCensusCBSA <- function() {

  # ----- Setup ----------------------------------------------------------------

  loadSpatialData("USCensusStates")

  # Use package internal data directory
  dataDir <- getSpatialDataDir()

  # Specify the name of the dataset and file being created
  datasetName <- 'USCensusCBSA'

  # ----- Get the data ---------------------------------------------------------

  # Build appropriate request URL for US County Borders data
  url <- 'https://www2.census.gov/geo/tiger/TIGER2021/CBSA/tl_2021_us_cbsa.zip'

  filePath <- file.path(dataDir, basename(url))
  utils::download.file(url, filePath)
  # NOTE:  This zip file has no directory so extra subdirectory needs to be created
  utils::unzip(filePath, exdir = file.path(dataDir, 'cbsa'))

  # ----- Convert to SFDF ------------------------------------------------------

  # Convert shapefile into simple features data frame
  # NOTE:  The 'cbsa' directory has been created
  dsnPath <- file.path(dataDir,'cbsa')
  shpName <- 'tl_2021_us_cbsa'
  SFDF <- convertLayer(
    dsn = dsnPath,
    layer = shpName
  )

  # ----- Select useful columns and rename -------------------------------------

  # > dplyr::glimpse(SFDF, width = 75)
  # Rows: 939
  # Columns: 13
  # $ CSAFP    <chr> "122", "122", "428", "426", "258", "532", "194", NA, NA,…
  # $ CBSAFP   <chr> "12020", "12060", "12100", "12120", "12140", "12180", "1…
  # $ GEOID    <chr> "12020", "12060", "12100", "12120", "12140", "12180", "1…
  # $ NAME     <chr> "Athens-Clarke County, GA", "Atlanta-Sandy Springs-Alpha…
  # $ NAMELSAD <chr> "Athens-Clarke County, GA Metro Area", "Atlanta-Sandy Sp…
  # $ LSAD     <chr> "M1", "M1", "M1", "M2", "M2", "M2", "M1", "M1", "M2", "M…
  # $ MEMI     <chr> "1", "1", "1", "2", "2", "2", "1", "1", "2", "2", "1", "…
  # $ MTFCC    <chr> "G3110", "G3110", "G3110", "G3110", "G3110", "G3110", "G…
  # $ ALAND    <dbl> 2654607902, 22495873026, 1438775279, 2448595161, 9397319…
  # $ AWATER   <dbl> 26109459, 386782308, 301270067, 20024887, 2657419, 44569…
  # $ INTPTLAT <chr> "+33.9439840", "+33.6937280", "+39.4693555", "+31.122286…
  # $ INTPTLON <chr> "-083.2138965", "-084.3999113", "-074.6337591", "-087.16…
  # $ geometry <MULTIPOLYGON [°]> MULTIPOLYGON (((-83.36003 3..., MULTIPOLYGO…

  #
  # Data Dictionary:
  #   $ CSAFP  ----->  (drop)
  #   $ CBSAFP  -----> CBSAFP
  #   $ GEOID   -----> (drop)
  #   $ NAME     -----> CBSAName
  #   $ NAMELSAD -----> (drop)
  #   $ LSAD     -----> (drop)
  #   $ MEMI    -----> sizeClass
  #   $ MTFCC   -----> (drop)
  #   $ ALAND    -----> landArea
  #   $ AWATER  -----> waterArea
  #   $ INTPTLAT -----> latitude
  #   $ INTPTLON -----> longitude

  # Convert lat/lon to numeric
  SFDF$INTPTLAT <- as.numeric(SFDF$INTPTLAT)
  SFDF$INTPTLON <- as.numeric(SFDF$INTPTLON)

  # We can use longitude and latitude to get one state code for each polygon.
  # Validation plot -- check if lon/lat are polygon centroids
  if ( FALSE ) {
    tx <- dplyr::filter(SFDF, stringr::str_detect(SFDF$NAME, "TX"))
    plot(tx)
    points(tx$INTPTLON, tx$INTPTLAT, pch = 16, col = 'red')
  }

  SFDF$stateCode <- getStateCode(SFDF$INTPTLON, SFDF$INTPTLAT, datasetName = 'USCensusStates', useBuffering = TRUE)
  SFDF$countryCode <- "US"

  # Get CBSAName and allStateCodes from the CBSAName column
  nameMatrix <- stringr::str_split_fixed(SFDF$NAME, ',', 2)
  SFDF$CBSAName <- nameMatrix[, 1]
  # allStateCodes is a comma-separate list of stateCodes
  SFDF$allStateCodes <- stringr::str_trim( stringr::str_replace_all(nameMatrix[,2], '-',',') )

  # Convert MEMI to explicitly indicate Micropolitan and Metropolitan classes
  metroMask <- SFDF$MEMI == "1"
  SFDF$MEMI[metroMask] <- "metro"
  SFDF$MEMI[!metroMask] <- "micro"

  # Create the new dataframe in a specific column order
  SFDF <-
    dplyr::select(
      .data = SFDF,
      countryCode = .data$countryCode,
      stateCode = .data$stateCode,
      allStateCodes = .data$allStateCodes,
      CBSAFP = .data$CBSAFP,
      CBSAName = .data$NAME,
      sizeClass = .data$MEMI,
      landArea = .data$ALAND,
      waterArea = .data$AWATER,
      latitude = .data$INTPTLAT,
      longitude = .data$INTPTLON
    )

  # ----- Simplify and save ----------------------------------------------------

  uniqueIdentifier <- "CBSAFP"

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
