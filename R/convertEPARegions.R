#' @importFrom rlang .data
#' @export
#'
#' @title Convert EPA Region shapefiles
#'
#' @description Returns a simple features data frame for EPA Regions
#'
#' The full resolution file will be named "EPA_Regions.rda". In addition,
#' "_05", _02" and "_01" versions of the file will be created that that are
#' simplified to 5\%, 2\% and 1\%. Simplified versions will greatly improve the
#' speed of both searching and plotting.
#'
#' @details An EPA region boundary shapefile is converted to a
#' simple features data frame with additional columns of data. The resulting file
#' will be created in the spatial data directory which is set with
#' \code{setSpatialDataDir()}.
#'
#' The source data is from 2022.
#'
#' @note From the source documentation:
#'
#' This datasetName represents delineated EPA Region boundaries. EPA has ten
#' regional offices across the country, each of which is responsible for several
#' states and in some cases, territories or special environmental programs.
#'
#' This Shared Enterprise Geodata and Services (SEGS)datasetName was created by
#' U.S. EPA using 2011 TIGER/Line state boundaries from the U.S. Census Bureau.
#' The core mission of SEGS is to provide a single point of ownership for
#' geospatial datasetNames that are national in extent and of general use to all
#' EPA users and to make those datasetNames available through channels that best
#' meet user needs.
#'
#' @return Name of the datasetName being created.
#'
#' @references \url{https://hub.arcgis.com/datasets/geoplatform::epa-regions}
#'

convertEPARegions <- function() {

  # ----- Setup ---------------------------------------------------------------

  loadSpatialData("USCensusStates")

  # Use package internal data directory
  dataDir <- getSpatialDataDir()

  # Specify the name of the dataset and file being created
  datasetName <- "EPARegions"

  # ----- Get the data ---------------------------------------------------------

  # # Build appropriate request URL
  # url <- 'https://opendata.arcgis.com/datasetNames/c670540796584c72b4f59b676ccabe6a_3.zip'
  #
  # filePath <- file.path(dataDir, basename(url))
  # utils::download.file(url, filePath)
  # # NOTE:  This zip file has no directory so extra subdirectory needs to be created
  # utils::unzip(filePath, exdir = file.path(dataDir, 'epa_regions'))

  # NOTE:  This shape file must now be downloaded and unzipped by hand from:
  # NOTE:    https://hub.arcgis.com/maps/geoplatform::epa-regions/about

  # ----- Convert to SFDF ------------------------------------------------------

  # Convert shapefile into simple features data frame
  # NOTE:  The 'epa_regions' directory has been created
  dsnPath <- file.path(dataDir, 'EPA_Regions')
  shpName <- 'EPA_Regions'
  SFDF <- convertLayer(
    dsn = dsnPath,
    layer = shpName
  )

  # ----- Select useful columns and rename -------------------------------------

  # > dplyr::glimpse(SFDF, width = 75)
  # Rows: 10
  # Columns: 5
  # $ OBJECTID   <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
  # $ EPAREGION  <chr> "Region 1", "Region 10", "Region 2", "Region 3", "Regi…
  # $ Shape__Are <dbl> 3.623687e+11, 1.033551e+13, 3.199246e+11, 5.526602e+11…
  # $ Shape__Len <dbl> 4318560, 41961155, 4949721, 4027496, 9424781, 9037567,…
  # $ geometry   <MULTIPOLYGON [°]> MULTIPOLYGON (((-71.58409 4..., MULTIPOLYGON (((-122.2…

  # Data dictionary:
  #   $ OBJECTID ---> polygonID: unique identifier
  #   $ EPAREGION --> name: the name of the region
  #   $ Shape__Len --> (drop)
  #   $ Shape__Are --> (drop)

  # Add core metadata
  SFDF$countryCode <- "US"

  # Get latitude and longitude from polygon centroids
  centroids <- sf::st_centroid(SFDF)
  lon <- sf::st_coordinates(centroids)[,1]
  lat <- sf::st_coordinates(centroids)[,2]

  SFDF$longitude <- lon
  SFDF$latitude <- lat

  # Use longitude and latitude to get one state code for each polygon.
  # NOTE: EPA Regions can span multiple states and include overseas territories
  SFDF$stateCode <- getStateCode(
    SFDF$longitude,
    SFDF$latitude,
    datasetName = 'USCensusStates',
    useBuffering = TRUE
  )

  # NOTE:  No good table exists so we create allStateCodes by hand from
  # NOTE:    https://www.epa.gov/aboutepa/visiting-regional-office
  allStateCodes <- list(
    "Region 1" = "CT,MA,ME,NH,RI,VT",
    "Region 2" = "NJ,NY,PR,VI",
    "Region 3" = "DC,DE,MD,PA,VA,WV",
    "Region 4" = "AL,FL,GA,KY,MS,NC,SC,TN",
    "Region 5" = "IL,IN,MI,MN,OH,WI",
    "Region 6" = "AR,LA,NM,OK,TX",
    "Region 7" = "IA,KS,MO,NE",
    "Region 8" = "CO,MT,ND,SD,UT,WY",
    "Region 9" = "AZ,CA,HI,NV",
    "Region 10" = "AK,ID,OR,WA"
  )

  # NOTE:  Super hacky/old school way to get an allStateCodes column
  df <- as.data.frame(as.matrix(allStateCodes))
  SFDF$allStateCodes <-
    as.character( df[SFDF$EPAREGION,] )

  # Create the new dataframe in a specific column order
  # NOTE: Not adding SFDF$allStateCodes because of overseas territory coverage.
  SFDF <-
    dplyr::select(
      .data = SFDF,
      countryCode = .data$countryCode,
      stateCode = .data$stateCode,
      allStateCodes = .data$allStateCodes,
      epaRegion = .data$EPAREGION,
      longitude = .data$longitude,
      latitude = .data$latitude
    )

  # ----- Simplify and save ----------------------------------------------------

  uniqueIdentifier <- "epaRegion"

  simplifyAndSave(
    SFDF = SFDF,
    datasetName = datasetName,
    uniqueIdentifier = uniqueIdentifier,
    dataDir = dataDir
  )

  # ----- Clean up and return --------------------------------------------------

  # NOTE:  The source file was manually downloaded

  # # Clean up
  # unlink(filePath, force = TRUE)
  # unlink(dsnPath, recursive = TRUE, force = TRUE)
  message("You may delete the manually downloaded source data.")

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
