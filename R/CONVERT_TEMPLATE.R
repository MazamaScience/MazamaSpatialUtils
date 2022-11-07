#' @export
#'
#' @title Convert TODO shapefile
#'
#' @return Name of the dataset being created.
#'
#' @description Returns a simple features data frame for TODO
#'
#' The full resolution file will be named "TODO". In addition,
#' "_05", _02" and "_01" versions of the file will be created that that are
#' simplified to 5\%, 2\% and 1\%. Simplified versions will greatly improve the
#' speed of both searching and plotting.
#'
#' The TODO layer is a polygon shapefile coverage representing TODO.
#'
#' TODO: More details
#'
#' @note TODO: MONTH, YEAR version.
#'
#' @references \url{TODO: DATA_URL}

# convertTODO <- function() {
#
#   # ----- Setup ---------------------------------------------------------------
#
#   # Use package internal data directory
#   dataDir <- getSpatialDataDir()
#
#   # NOTE:  Dataset names should be lowerCamelCase with now abbreviations
#   # NOTE:  except for known acronymes.
#
#   # Specify the name of the dataset and file being created
#   datasetName <- "TODO"
#
#   # ----- Get the data ---------------------------------------------------------
#
#   # NOTE:  Ideally, data can be downloaded from a spcific URL as a .zip file.
#   # NOTE:  In this case, the following example code is a good template.
#
#   # # Build appropriate request URL
#   # url <- 'https://www.arb.ca.gov/ei/gislib/boundaries/ca_air_basins.zip'
#   #
#   # filePath <- file.path(dataDir,basename(url))
#   # utils::download.file(url,filePath)
#   # # NOTE:  This zip file has no directory so extra subdirectory needs to be created
#   # utils::unzip(filePath,exdir=file.path(dataDir, 'ca_air_basins'))
#
#   # ----- Convert to SFDF ------------------------------------------------------
#
#   # NOTE:  You have to look at the downloaded data to determin shpName
#
#   # Convert shapefile into simple features data frame
#   # NOTE:  The 'world' directory has been created
#   dsnPath <- file.path(dataDir, 'TODO')
#   shpName <- 'TODO'
#   SFDF <- convertLayer(
#     dsn = dsnPath,
#     layer = shpName
#   )
#
#   # ----- Select useful columns and rename -------------------------------------
#
#   # NOTE:  Convert names to human readable, unabbreviated, lowerCamelCase names.
#   # NOTE:  Reasonable names will be constructed as "subTypeNoun" e.g.:
#   # NOTE:    waterSurfaceArea, landSurfaceArea, k12StudentCount, collegeStudentCount
#
#   # > dplyr::glimpse(SFDF, width = 75)
#   # TODO:  PASTE ORIGINAL RESULTS HERE
#
#   # TODO Example from convertWeatherZones.R
#   #
#   # SFDF <- dplyr::select(
#   #   SFDF,
#   #   stateCode = .data$STATE,
#   #   weatherForecastOffice = .data$CWA,
#   #   zoneNumber = .data$ZONE,
#   #   name = .data$NAME,
#   #   zoneID = .data$STATE_ZONE,
#   #   longitude = .data$LON,
#   #   latitude = .data$LAT
#   # )
#
#   # ----- Convert to standard (metric) units -----------------------------------
#
#   # NOTE:  Some datasetNames should have some variables, typically areas, converted
#   # NOTE:  to metric m^2.  Other units will be converted on a case-by-case
#   # NOTE:  basis depending on the needs/expectations of any potential user
#   # NOTE:  community. (judgement call)
#
#   # TODO
#
#   # ----- Add country and state codes ------------------------------------------
#
#   # NOTE:  Several functions allow filtering by countryCode and stateCode as a
#   # NOTE:  way of reducing the number of polygons that need to be searched.
#   # NOTE:  These variables should be included inevery datasetName.
#   # NOTE:
#   # NOTE:  These might be assumed or read from a column (that might be coded in
#   # NOTE:  some other way). Or, you might have to use MazamaSpatialUtils::getStateCode()
#   # NOTE:  on the polygon centers.
#
#   SFDF$countryCode <- "US" # TODO?
#   SFDF$stateCode <- "CA"   # TODO?
#
#   # NOTE:  Some datasetNames may also wish to include SFDF$allStateCodes to show
#   # NOTE:  all of the states that overlap with each polygon. An example where
#   # NOTE:  this is done is convertWBDHUC.R. (judgement call)
#
#   # ----- Simplify and save ----------------------------------------------------
#
#   uniqueIdentifier <- "stateFIPS"
#
#   simplifyAndSave(
#     SFDF = SFDF,
#     datasetName = datasetName,
#     uniqueIdentifier = uniqueIdentifier,
#     dataDir = dataDir
#   )
#
#   # ----- Clean up and return --------------------------------------------------
#
#   unlink(filePath, force = TRUE)
#   unlink(dsnPath, recursive = TRUE, force = TRUE)
#
#   return(invisible(datasetName))
#
# }
#
# # ===== TEST ===================================================================
#
# if ( FALSE ) {
#
#   library(sf)
#
#   # Look or horizontal lines from polygons that cross the dateline.
#   # NOTE:  These are sometimes created by sf::st_make_valid()
#   loadSpatialData(datasetName)
#   SFDF <- get(paste0(datasetName, ""))
#   SFDF_05 <- get(paste0(datasetName, "_05"))
#   SFDF_02 <- get(paste0(datasetName, "_02"))
#   SFDF_01 <- get(paste0(datasetName, "_01"))
#
#   plot(SFDF_01$geometry)
#   dev.off(dev.list()["RStudioGD"])
#   plot(SFDF_02$geometry)
#   dev.off(dev.list()["RStudioGD"])
#   plot(SFDF_05$geometry)
#   dev.off(dev.list()["RStudioGD"])
#   #plot(SFDF$geometry)
#
#   # Try out getSpatialData()
#   lons <- c(-120:-110, 0:10)
#   lats <- c(30:40, 30:40)
#
#   df <- getSpatialData(lons, lats, SFDF_01)
#   df <- getSpatialData(lons, lats, SFDF_02)
#   df <- getSpatialData(lons, lats, SFDF_05)
#   df <- getSpatialData(lons, lats, SFDF)
#
#   # Special Case of Russian failing to plot properly
#   SFDF %>% dplyr::filter(countryCode == "RU") %>% sf::st_geometry() %>% plot()
#
# }
