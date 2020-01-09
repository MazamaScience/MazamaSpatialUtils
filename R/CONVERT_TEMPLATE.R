#' @keywords datagen
#' @export
#' @title Convert XXX shapefiles
#' @param nameOnly Logical specifying whether to only return the name without 
#' creating the file.
#' @param simplify Logical specifying whether to create "_05", _02" and "_01" 
#' versions of the file that are simplified to 5\%, 2\% and 1\%.
#' @return Name of the dataset being created.
#' @description Returns a SpatialPolygonsDataFrame for XXX
#' 
#' The XXX layer is a polygon shapefile coverage representing XXX
#' XXX
#' XXX
#' XXX
#' @note MONTH, YEAR version.
#' @references \url{DATA URL}

# convertXXX <- function(
#   nameOnly = FALSE,
#   simplify = FALSE
# ) {
#   
#   # ----- Setup ---------------------------------------------------------------
#   
#   # Use package internal data directory
#   dataDir <- getSpatialDataDir()
#   
#   # Specify the name of the dataset and file being created
#   datasetName <- "XXX"
#   
#   if (nameOnly) 
#     return(datasetName)
#   
#   # ----- Get the data ---------------------------------------------------------
#   
#   # Build appropriate request URL
#   url <- 'https://www.arb.ca.gov/ei/gislib/boundaries/ca_air_basins.zip'
#   
#   filePath <- file.path(dataDir,basename(url))
#   utils::download.file(url,filePath)
#   # NOTE:  This zip file has no directory so extra subdirectory needs to be created
#   utils::unzip(filePath,exdir = file.path(dataDir, "XXX"))
#   
#   # ----- Convert to SPDF ------------------------------------------------------
#   
#   # Convert shapefile into SpatialPolygonsDataFrame
#   
#   dsnPath <- file.path(dataDir, "XXX")
#   shpName <- 'CaAirBasin'
#   SPDF <- convertLayer(dsn = dsnPath, layerName = shpName)
#   
#   # ----- Select useful columns and rename -------------------------------------
#   
#   # Human readable, lowerCamelCase names
#   
#   # > head(SPDF@data, 5)
#   # PASTE ORIGINAL RESULTS HERE
#   
#   # XXX Example from convertWeatherZones.R
#   #
#   # SPDF@data <- dplyr::select(
#   #   SPDF@data,
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
#   # NOTE:  Only for datasets with "funky" units (judgement call)
#   
#   # XXX
#   
#   # ----- Organize polygons ----------------------------------------------------
#   
#   # NOTE:  Only for datasets where the "unique ID" is duplicated
#   
#   # XXX Example from convertWeatherZones.R
#   #
#   # duplicated <- SPDF$zoneID[duplicated(SPDF$zoneID)]
#   # SPDF <- organizePolygons(SPDF, "zoneID", sumColumns = c("longitude", "latitude"))
#   # 
#   # # Get correct lat/lon centroids for new polygons
#   # data <- SPDF@data
#   # centroids <- rgeos::gCentroid(subset(SPDF, SPDF$zoneID %in% duplicated), byid=TRUE)
#   # for (id in duplicated) {
#   #   data[data$zoneID == id,]$longitude <- centroids[id]$x
#   #   data[data$zoneID == id,]$latitude <- centroids[id]$y
#   # }
#   # SPDF@data <- data
#   
#   # ----- Add country and state codes ------------------------------------------
#   
#   # NOTE:  These might be assumed or read from a column (that might be coded in
#   # NOTE:  some other way). Or, you might have to use MazamaSpatialUtils::getStateCode()
#   # NOTE:  on the polygon centers.
#   
#   SPDF$countryCode <- "US" # XXX?
#   SPDF$stateCode <- "CA"   # XXX?
#   
#   # ----- Name and save the data -----------------------------------------------
#   
#   message("Saving full resolution version...\n")
#   assign(datasetName, SPDF)
#   save(list = c(datasetName), file = paste0(dataDir,'/',datasetName,'.RData'))
#   rm(list = datasetName)
#   
#   # ----- Simplify -------------------------------------------------------------
#   
#   if ( simplify ) {
#     # Create new, simplified datsets: one with 5%, 2%, and one with 1% of the vertices of the original
#     # NOTE:  This may take several minutes. 
#     message("Simplifying to 5%...\n")
#     SPDF_05 <- rmapshaper::ms_simplify(SPDF, 0.05)
#     SPDF_05@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
#     datasetName_05 <- paste0(datasetName, "_05")
#     message("Saving 5% version...\n")
#     assign(datasetName_05, SPDF_05)
#     save(list = datasetName_05, file = paste0(dataDir,"/",datasetName_05, '.RData'))
#     rm(list = c("SPDF_05",datasetName_05))
#     
#     message("Simplifying to 2%...\n")
#     SPDF_02 <- rmapshaper::ms_simplify(SPDF, 0.02)
#     SPDF_02@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
#     datasetName_02 <- paste0(datasetName, "_02")
#     message("Saving 2% version...\n")
#     assign(datasetName_02, SPDF_02)
#     save(list = datasetName_02, file = paste0(dataDir,"/",datasetName_02, '.RData'))
#     rm(list = c("SPDF_02",datasetName_02))
#     
#     message("Simplifying to 1%...\n")
#     SPDF_01 <- rmapshaper::ms_simplify(SPDF, 0.01) 
#     SPDF_01@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
#     datasetName_01 <- paste0(datasetName, "_01")
#     message("Saving 1% version...\n")
#     assign(datasetName_01, SPDF_01)
#     save(list = datasetName_01, file = paste0(dataDir,"/",datasetName_01, '.RData'))
#     rm(list = c("SPDF_01",datasetName_01))
#   }
#   
#   # ----- Clean up and return --------------------------------------------------
#   
#   unlink(filePath, force = TRUE)
#   unlink(dsnPath, recursive = TRUE, force = TRUE)
#   
#   return(invisible(datasetName))
#   
# }

