#' @importFrom rlang .data
#' @export
#'
#' @title Convert US county borders shapefile
#'
#' @description Create a simple features data frame for US counties.
#'
#' The full resolution file will be named "USCensusCounties.rda". In addition,
#' "_05", _02" and "_01" versions of the file will be created that that are
#' simplified to 5\%, 2\% and 1\%. Simplified versions will greatly improve the
#' speed of both searching and plotting.
#'
#' @details A US county borders shapefile is downloaded and converted to a
#' simple features data frame with additional columns of data. The resulting file
#' will be created in the spatial data directory which is set with
#' \link{setSpatialDataDir}.
#'
#' The source data is from 2021.
#'
#' @note From the source documentation:
#'
#' The primary legal divisions of most states are termed counties. In Louisiana,
#' these divisions are known as parishes. In Alaska, which has no counties, the
#' equivalent entities are the organized boroughs, city and boroughs,
#' municipalities, and for the unorganized area, census areas. The latter are
#' delineated cooperatively for statistical purposes by the State of Alaska and
#' the Census Bureau. In four states (Maryland, Missouri, Nevada, and Virginia),
#' there are one or more incorporated places that are independent of any county
#' organization and thus constitute primary divisions of their states. These
#' incorporated places are known as independent cities and are treated as
#' equivalent entities for purposes of data presentation. The District of
#' Columbia and Guam have no primary divisions, and each area is considered an
#' equivalent entity for purposes of data presentation. The Census Bureau treats
#' the following entities as equivalents of counties for purposes of data
#' presentation: Municipios in Puerto Rico, Districts and Islands in American
#' Samoa, Municipalities in the Commonwealth of the Northern Mariana Islands,
#' and Islands in the U.S. Virgin Islands. The entire area of the United States,
#' Puerto Rico, and the Island Areas is covered by counties or equivalent entities.
#'
#' You can join this file with table data downloaded from American FactFinder by
#' using the AFFGEOID field in the cartographic boundary file.
#'
#' @return Name of the datasetName being created.
#'
#' @references \url{https://www2.census.gov/geo/tiger/GENZ2021/}
#'

convertUSCensusCounties <- function() {

  # ----- Setup ----------------------------------------------------------------

  # Use package internal data directory
  dataDir <- getSpatialDataDir()

  # Specify the name of the dataset and file being created
  datasetName <- 'USCensusCounties'

  # ----- Get the data ---------------------------------------------------------

  # Build appropriate request URL
  # NOTE: 500k means resolution level 1:500k.
  url <- 'https://www2.census.gov/geo/tiger/GENZ2021/shp/cb_2021_us_county_500k.zip'

  filePath <- file.path(dataDir,basename(url))
  utils::download.file(url,filePath)
  # NOTE:  This zip file has no directory so extra subdirectory needs to be created
  utils::unzip(filePath, exdir = file.path(dataDir, 'counties'))

  # ----- Convert to SFDF ------------------------------------------------------

  # Convert shapefile into simple features data frame
  # NOTE:  The 'counties' directory has been created
  dsnPath <- file.path(dataDir, 'counties')
  shpName <- 'cb_2021_us_county_500k'
  SFDF <- convertLayer(
    dsn = dsnPath,
    layer = shpName
  )

  # ----- Select useful columns and rename -------------------------------------

  # > dplyr::glimpse(SFDF, width = 75)
  # Rows: 3,234
  # Columns: 13
  # $ STATEFP    <chr> "20", "19", "30", "16", "55", "31", "08", "42", "40", …
  # $ COUNTYFP   <chr> "161", "159", "009", "007", "011", "185", "037", "129"…
  # $ COUNTYNS   <chr> "00485044", "00465268", "01720111", "00395090", "01581…
  # $ AFFGEOID   <chr> "0500000US20161", "0500000US19159", "0500000US30009", …
  # $ GEOID      <chr> "20161", "19159", "30009", "16007", "55011", "31185", …
  # $ NAME       <chr> "Riley", "Ringgold", "Carbon", "Bear Lake", "Buffalo",…
  # $ NAMELSAD   <chr> "Riley County", "Ringgold County", "Carbon County", "B…
  # $ STUSPS     <chr> "KS", "IA", "MT", "ID", "WI", "NE", "CO", "PA", "OK", …
  # $ STATE_NAME <chr> "Kansas", "Iowa", "Montana", "Idaho", "Wisconsin", "Ne…
  # $ LSAD       <chr> "06", "06", "06", "06", "06", "06", "06", "06", "06", …
  # $ ALAND      <dbl> 1579077672, 1386932347, 5303728455, 2527123155, 175029…
  # $ AWATER     <dbl> 32047392, 8723135, 35213028, 191364281, 87549529, 8595…
  # $ geometry   <MULTIPOLYGON [°]> MULTIPOLYGON (((-96.96095 3..., MULTIPOLY…

  # Data Dictionary:
  #   STATEFP -----> stateFIPS: 2-digit FIPS code
  #   COUNTYFP ----> combined with STATEFP to make countyFIPS
  #   COUNTYNS ----> COUNTYNS
  #   AFFGEOID ----> AFFGEOID
  #   GEOID -------> (drop)
  #   NAME --------> countyName: English language name
  #   NAMELSAD ----> (drop)
  #   STUSPS ------> stateCode
  #   STATE_NAME --> stateName
  #   LSAD --------> (drop)
  #   ALAND -------> landArea: land area (in sq. meters)
  #   AWATER ------> waterArea: water area (in sq. meters)

  # Guarantee that ALAND and AWATER are numeric
  SFDF$ALAND <- as.numeric(SFDF$ALAND)
  SFDF$AWATER <- as.numeric(SFDF$AWATER)

  SFDF$countryCode <- "US"
  SFDF$stateCode <- SFDF$STUSPS
  SFDF$countyFIPS <- paste0(SFDF$STATEFP, SFDF$COUNTYFP)

  # Remove outlying territories
  SFDF <- dplyr::filter(SFDF, .data$stateCode %in% US_52)

  # Create the new dataframe in a specific column order
  SFDF <-
    SFDF %>%
    dplyr::select(
      countryCode = .data$countryCode,
      stateCode = .data$stateCode,
      stateFIPS = .data$STATEFP,
      stateName = .data$STATE_NAME,
      countyName = .data$NAME,
      countyFIPS = .data$countyFIPS,
      landArea = .data$ALAND,
      waterArea = .data$AWATER,
      COUNTYNS = .data$COUNTYNS,
      AFFGEOID = .data$AFFGEOID
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
