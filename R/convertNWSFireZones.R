#' @keywords datagen
#' @export
#' @title Convert NWS Public Forecast Zones Shapefile
#' @param nameOnly logical specifying whether to only return the name without creating the file
#' @param simplify logical specifying whether to create "_05" version of the file that is simplified to 5\% 
#' @description Returns a SpatialPolygonsDataFrame for NWS weather forecast zones.
#' @details A weather forecast zones shapefile is downloaded and converted to a
#' SpatialPolygonsDataFrame with additional columns of data. The resulting file will be created
#' in the spatial data directory which is set with \code{setSpatialDataDir()}.
#' @note zoneID is the unique identifier, and is the state code followed by zoneNumber. 
#' @return Name of the dataset being created.
#' @references \url{https://www.weather.gov/gis/FireZones}
#' @seealso setSpatialDataDir

convertNWSFireZones <- function(nameOnly = FALSE, simplify = TRUE) {
  
  # Use package internal data directory
  dataDir <- getSpatialDataDir()
  
  # Specify the name of the dataset and file being created
  datasetName <- 'NWSFireZones'
  
  if (nameOnly) return(datasetName)
  
  # Build appropriate request URL for TM World Borders data
  url <- "https://www.weather.gov/source/gis/Shapefiles/WSOM/fz02oc18.zip"
  filePath <- file.path(dataDir, "NWSFireZones.zip")
  utils::download.file(url,filePath)
  
  # NOTE:  This zip file has no directory so extra subdirectory needs to be created
  utils::unzip(filePath,exdir=file.path(dataDir,'NWSFireZones'))
  dsnPath <- file.path(dataDir,'NWSFireZones')
  SPDF <- convertLayer(dsn=dsnPath, layerName='fz02oc18')
  
  # > names(SPDF)
  # [1] "STATE"      "ZONE"       "CWA"        "NAME"       "STATE_ZONE" "TIME_ZONE"  "FE_AREA"    "LON"       
  # [9] "LAT"   
  
  # Relabel and standardize the naming in the SpatialPolygonsDataFrame
  
  
  # NOTE:  The values in the TIME_ZONE field do not correspond to timezones from the 
  # NOTE:  WorldTimezones file so that field will be dropped to avoid confusion.
  # NOTE:  For example:
  # NOTE:  loadSpatialData("WorldTimezones")
  # NOTE:  plot(subset(SPDF, TIME_ZONE == "A"))
  # NOTE:  plot(subset(WorldTimezones, timezone == "America/Anchorage"), border = "red", add = TRUE)
  # NOTE:  plot(subset(WorldTimezones, timezone == "America/Nome"), border = "blue", add = TRUE)
  
  
  SPDF@data <- dplyr::select(.data = SPDF@data,
                      stateCode = .data$STATE,
                      weatherForecastOffice = .data$CWA,
                      zoneNumber = .data$ZONE,
                      name = .data$NAME,
                      zoneID = .data$STATE_ZONE,
                      longitude = .data$LON,
                      latitude = .data$LAT)
  
  
  # Organize polygons
  duplicated <- SPDF$zoneID[duplicated(SPDF$zoneID)]
  SPDF <- organizePolygons(SPDF, "zoneID", sumColumns = c("longitude", "latitude"))
  
  # Get correct lat/lon centroids for new polygons
  data <- SPDF@data
  centroids <- rgeos::gCentroid(subset(SPDF, SPDF$zoneID %in% duplicated), byid=TRUE)
  for (id in duplicated) {
    data[data$zoneID == id,]$longitude <- centroids[id]$x
    data[data$zoneID == id,]$latitude <- centroids[id]$y
  }
  SPDF@data <- data
  
  # Assign name and save the data
  assign(datasetName,SPDF)
  save(list=datasetName, file = paste0(dataDir,"/",datasetName, '.RData'))
  rm(list=datasetName)
  
  
  if ( simplify ) {
    
    SPDF_05 <- rmapshaper::ms_simplify(SPDF, .05)
    datasetName_05 <- paste0(datasetName, "_05")
    assign(datasetName_05, SPDF_05)
    save(list = datasetName_05, file = paste0(dataDir, "/", datasetName_05, ".RData"))
    
  }
  
  return(invisible(datasetName))  
  
}

