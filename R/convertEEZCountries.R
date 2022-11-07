#' @importFrom rlang .data
#' @export
#'
#' @title Convert Exclusive Economic Zones countries shapefile
#'
#' @description Create a simple features dataframe for combined EEZ/country boundaries.
#'
#' The full resolution file will be named "EEZCountries.rda". In addition,
#' "_05", _02" and "_01" versions of the file will be created that that are
#' simplified to 5\%, 2\% and 1\%. Simplified versions will greatly improve the
#' speed of both searching and plotting.
#'
#' @details A world EEZ/countries shapefile is converted to a simple features dataframe
#' with additional columns of data. To use this function, the file
#' "EEZ_land_union_v3_202003.zip" must be downloaded into the user's spatial
#' directory which is set with \code{setSpatialDataDir()}. The resulting file
#' will be created in this same spatial data directory.
#'
#' @note For polygons with overlapping claims of sovereignty, we arbitrarily
#' assign the polygon to the country identified in the \code{ISO_SOV1} field.
#'
#' The source data is from Version 3 -- 2020-03-17.
#'
#' @note From the source documentation:
#'
#' Geographic Information Systems have become indispensable tools in managing
#' and displaying marine data and information. However, a unique georeferenced
#' standard of marine place names and areas was not available, hampering several
#' marine geographic applications, for example the linking of these locations to
#' databases to integrate data. The purpose of Marine Regions is therefore to
#' create a standard, relational list of geographic names, coupled with information
#' and maps of the geographic location of these features. This will improve access
#' and clarity of the different geographic, marine names such as seas, sandbanks,
#' ridges and bays and display univocally the boundaries of marine biogeographic
#' or managerial marine areas.
#'
#' Marine Regions is an integration of the VLIMAR Gazetteer and the VLIZ Maritime
#' Boundaries Geodatabase. The VLIMAR Gazetteer is a database with geographic,
#' mainly marine names such as seas, sandbanks, seamounts, ridges, bays or even
#' standard sampling stations used in marine research. The geographic cover of
#' the VLIMAR gazetteer is global but initially focused on the Belgian Continental
#' Shelf and the Scheldt Estuary and the Southern Bight of the North Sea. Gradually
#' more regional and global geographic information was added to VLIMAR and combining
#' this information with the Maritime Boundaries database, representing the
#' Exclusive Economic Zone (EEZ) of the world, led to the creation of marineregions.org.
#'
#' Marine Regions is managed by the Flanders Marine Institute. Funding for the
#' creation of the VLIMAR gazetteer was provided initially through the EU Network
#' of Excellence MarBEF, but also other European initiatives such as Lifewatch
#' provide the necessary funding for the maintenance and management of Marine Regions.
#'
#' Marine Regions depends on data and knowledge sharing from global, European,
#' regional and national data providers and relevant experts. By setting up
#' Collaboration Agreements, data providers will benefit from belonging to the
#' Marine Regions partnership as they would get increased visibility, gain access
#' to a variety of data analysis services which will benefit from integration of
#' several distributed spatial datasetNames, as well as enjoying the benefit of the
#' creation of stable unique identifiers. An example template of a Collaboration
#' Agreement can be found here. Please contact info@marineregions.org if your
#' organisation is interested to explore this collaboration.
#'
#' Citation:
#' Flanders Marine Institute (2020). Union of the ESRI Country shapefile and the
#' Exclusive Economic Zones (version 3). Available online at
#' https://www.marineregions.org/. https://doi.org/10.14284/403
#'#'
#' @return Name of the datasetName being created.
#'
#' @references \url{https://www.marineregions.org/sources.php#unioneezcountry}
#'
#' @seealso setSpatialDataDir
#'

convertEEZCountries <- function() {

  # ----- Setup ----------------------------------------------------------------

  # Use package internal data directory
  dataDir <- getSpatialDataDir()

  # Specify the name of the file being created
  datasetName <- 'EEZCountries'

  # ----- Get the data ---------------------------------------------------------

  # NOTE:  The source file must be manually downloaded andn unzipped

  # # Test if the shapefile directory exists.
  # filePath <- file.path(dataDir,'World_EEZ_v11_20191118_LR.zip')
  # if ( !file.exists(filePath) ) {
  #   stop('Shapefile directory does not exists. Please download and convert the shapefile desired.', call.=FALSE)
  # }
  #
  # # Unzip the downloaded file
  # utils::unzip(filePath, exdir = file.path(dataDir))

  # ----- Convert to SFDF ------------------------------------------------------

  # Convert shapefile into simple features data frame
  dsnPath <- file.path(dataDir, 'EEZ_land_union_v3_202003')
  shpName <- "EEZ_Land_v3_202030"
  SFDF <- convertLayer(
    dsn = dsnPath,
    layer = shpName
  )

  # ----- Select useful columns and rename -------------------------------------

  # > dplyr::glimpse(SFDF, width = 75)
  # Rows: 323
  # Columns: 31
  # $ UNION      <chr> "Estonia", "Mayotte", "Overlapping claim Qatar / Saudi…
  # $ MRGID_EEZ  <dbl> 5675, 48944, 50170, 8475, 5676, 8340, 8435, 8488, 4897…
  # $ TERRITORY1 <chr> "Estonia", "Mayotte", "Qatar", "Cameroon", "Finland", …
  # $ MRGID_TER1 <dbl> 2110, 8606, 8468, 2170, 2106, 31136, 3297, 17597, 2175…
  # $ ISO_TER1   <chr> "EST", "MYT", "QAT", "CMR", "FIN", "ATF", "FRO", "KIR"…
  # $ UN_TER1    <dbl> 233, 175, 634, 120, 246, 260, 234, 296, 170, 566, 586,…
  # $ SOVEREIGN1 <chr> "Estonia", "France", "Qatar", "Cameroon", "Finland", "…
  # $ MRGID_SOV1 <dbl> 2110, 17, 8468, 2170, 2106, 17, 2157, 2116, 2175, 2253…
  # $ ISO_SOV1   <chr> "EST", "FRA", "QAT", "CMR", "FIN", "FRA", "DNK", "KIR"…
  # $ UN_SOV1    <dbl> 233, 250, 634, 120, 246, 250, 208, 296, 170, 566, 586,…
  # $ TERRITORY2 <chr> NA, "Mayotte", "Saudi Arabia", NA, NA, NA, NA, NA, "Do…
  # $ MRGID_TER2 <dbl> 0, 8606, 2215, 0, 0, 0, 0, 0, 8640, 0, 0, 0, 2187, 0, …
  # $ ISO_TER2   <chr> NA, "MYT", "SAU", NA, NA, NA, NA, NA, "DOM", NA, NA, N…
  # $ UN_TER2    <dbl> NA, 175, 682, NA, NA, NA, NA, NA, 214, NA, NA, NA, 706…
  # $ SOVEREIGN2 <chr> NA, "Comores", "Saudi Arabia", NA, NA, NA, NA, NA, "Do…
  # $ MRGID_SOV2 <dbl> 0, 2163, 2215, 0, 0, 0, 0, 0, 8640, 0, 0, 0, 2187, 0, …
  # $ ISO_SOV2   <chr> NA, "COM", "SAU", NA, NA, NA, NA, NA, "DOM", NA, NA, N…
  # $ UN_SOV2    <dbl> NA, 174, 682, NA, NA, NA, NA, NA, 214, NA, NA, NA, 706…
  # $ TERRITORY3 <chr> NA, NA, "United Arab Emirates", NA, NA, NA, NA, NA, "V…
  # $ MRGID_TER3 <dbl> 0, 0, 2206, 0, 0, 0, 0, 0, 2201, 0, 0, 0, 0, 0, 0, 0, …
  # $ ISO_TER3   <chr> NA, NA, "ARE", NA, NA, NA, NA, NA, "VEN", NA, NA, NA, …
  # $ UN_TER3    <dbl> NA, 0, 784, NA, NA, NA, NA, NA, 862, NA, NA, NA, NA, N…
  # $ SOVEREIGN3 <chr> NA, NA, "United Arab Emirates", NA, NA, NA, NA, NA, "V…
  # $ MRGID_SOV3 <dbl> 0, 0, 2206, 0, 0, 0, 0, 0, 2201, 0, 0, 0, 0, 0, 0, 0, …
  # $ ISO_SOV3   <chr> NA, NA, "ARE", NA, NA, NA, NA, NA, "VEN", NA, NA, NA, …
  # $ UN_SOV3    <dbl> NA, 0, 784, NA, NA, NA, NA, NA, 862, NA, NA, NA, NA, N…
  # $ POL_TYPE   <chr> "Union EEZ and country", "Overlapping claim", "Overlap…
  # $ Y_1        <dbl> 58.71530, -13.21377, 24.69243, 5.62533, 63.97904, -20.…
  # $ x_1        <dbl> 24.40898, 45.30528, 51.59986, 12.63665, 25.44114, 39.4…
  # $ AREA_KM2   <dbl> 81842, 67285, 126, 480130, 420076, 120802, 267048, 105…
  # $ geometry   <MULTIPOLYGON [°]> MULTIPOLYGON (((26.61348 57..., MULTIPOLY…

  # Data Dictionary:
  #   UNION -------> claimants
  #   MRGID_EEZ --->
  #   TERRITORY1 -->
  #   MRGID_TER1 -->
  #   ISO_TER1 ---->
  #   UN_TER1 ----->
  #   SOVEREIGN1 --> countryName
  #   MRGID_SOV1 -->
  #   ISO_SOV1 ---->
  #   UN_SOV1 ----->
  #   TERRITORY2 -->
  #   MRGID_TER2 -->
  #   ISO_TER2 ---->
  #   UN_TER2 ----->
  #   SOVEREIGN2 -->
  #   MRGID_SOV2 -->
  #   ISO_SOV2 ---->
  #   UN_SOV2 ----->
  #   TERRITORY3 -->
  #   MRGID_TER3 -->
  #   ISO_TER3 ---->
  #   UN_TER3 ----->
  #   SOVEREIGN3 -->
  #   MRGID_SOV3 -->
  #   ISO_SOV3 ---->
  #   UN_SOV3 ----->
  #   POL_TYPE ---->
  #   Y_1 ---------> latitude
  #   x_1 ---------> longitude
  #   AREA_KM2 ----> area_km2

  oldNames <- names(SFDF)
  newNames <- c("countryCode", "countryName", "claimants", "longitude", "latitude", "area_km2")
  allNames <- c(newNames, oldNames)

  SFDF$claimants <- SFDF$UNION
  SFDF$countryCode <- iso3ToIso2(SFDF$ISO_SOV1)
  SFDF$countryName <- SFDF$SOVEREIGN1
  SFDF$longitude <- SFDF$x_1
  SFDF$latitude <- SFDF$Y_1
  SFDF$area_km2 <- as.numeric(SFDF$AREA_KM2)

  SFDF <- SFDF[,allNames]

  # ----- Simplify and save ----------------------------------------------------

  uniqueIdentifier <- "claimants"

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

  # Special Case of Russian failing to plot properly
  SFDF %>% dplyr::filter(countryCode == "RU") %>% sf::st_geometry() %>% plot()

}
