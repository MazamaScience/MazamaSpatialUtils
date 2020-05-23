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
#' @references \url{https://www.weather.gov/gis/PublicZones}
#' @seealso setSpatialDataDir

# ----- Setup ----------------------------------------------------------------

convertWeatherZones <- function(nameOnly = FALSE, simplify = TRUE) {
  
  # Use package internal data directory
  dataDir <- getSpatialDataDir()
  
  # Specify the name of the dataset and file being created
  datasetName <- 'weatherZones'
  
  if (nameOnly) return(datasetName)

  # ----- Get the data ---------------------------------------------------------
  
  # Build appropriate request URL for TM World Borders data
  url <- "https://www.weather.gov/source/gis/Shapefiles/WSOM/z_03mr20.zip"
  filePath <- file.path(dataDir, "weatherZones.zip")
  utils::download.file(url,filePath)
  
  # ----- Convert to SPDF ------------------------------------------------------
  
  # NOTE:  This zip file has no directory so extra subdirectory needs to be created
  utils::unzip(filePath,exdir=file.path(dataDir,'weatherZones'))
  dsnPath <- file.path(dataDir,'weatherZones')
  SPDF <- convertLayer(dsn=dsnPath, layerName='z_03mr20')
  
  # > names(SPDF)
  # [1] "STATE"      "CWA"        "TIME_ZONE"  "FE_AREA"    "ZONE"       "NAME"       "STATE_ZONE"
  # [8] "LON"        "LAT"        "SHORTNAME"  "InPoly_FID" "SimPgnFlag" "MaxSimpTol" "MinSimpTol"
  
  # > head(SPDF@data)
  # STATE CWA TIME_ZONE FE_AREA ZONE     NAME STATE_ZONE      LON    LAT SHORTNAME InPoly_FID SimPgnFlag MaxSimpTol MinSimpTol
  # 0    GU GUM         J    <NA>  013 Kayangel      GU013 134.7129 8.0736  Kayangel          1          0      1e-04      1e-04
  # 1    GU GUM         J    <NA>  011    Koror      GU011 134.5351 7.4442     Koror          2          0      1e-04      1e-04
  # 2    GU GUM         F    <NA>  041  Pohnpei      GU041 158.2252 6.8807   Pohnpei          3          0      1e-04      1e-04
  
  
  # Relabel and standardize the naming in the SpatialPolygonsDataFrame
  

  # NOTE:  The values in the TIME_ZONE field do not correspond to timezones from the 
  # NOTE:  WorldTimezones file so that field will be dropped to avoid confusion.
  # NOTE:  For example:
  # NOTE:  loadSpatialData("WorldTimezones")
  # NOTE:  plot(subset(weather_05, TIME_ZONE == "A"))
  # NOTE:  plot(subset(WorldTimezones, timezone == "America/Anchorage"), border = "red", add = TRUE)
  # NOTE:  plot(subset(WorldTimezones, timezone == "America/Nome"), border = "blue", add = TRUE)
  
  # ----- Select useful columns and rename -------------------------------------
  
  SPDF@data <- dplyr::select(SPDF@data,
                      stateCode = .data$STATE,
                      weatherForecastOffice = .data$CWA,
                      zoneNumber = .data$ZONE,
                      name = .data$NAME,
                      zoneID = .data$STATE_ZONE,
                      longitude = .data$LON,
                      latitude = .data$LAT)
  
  # ----- Organize polygons ----------------------------------------------------
  
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
  
  
  # ----- Name and save the data -----------------------------------------------
  
  # Assign name and save the data
  assign(datasetName,SPDF)
  save(list=datasetName, file = paste0(dataDir,"/",datasetName, '.RData'))
  rm(list=datasetName)
  
  
  # ----- Simplify -------------------------------------------------------------
  
  if ( simplify ) {
    
    SPDF_05 <- rmapshaper::ms_simplify(SPDF, .05)
    datasetName_05 <- paste0(datasetName, "_05")
    assign(datasetName_05, SPDF_05)
    save(list = datasetName_05, file = paste0(dataDir, "/", datasetName_05, ".RData"))
    
  }
  
  # ----- Clean up and return --------------------------------------------------
  
  return(invisible(datasetName))  
  
}

