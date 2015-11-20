#' @keywords datagen
#' @export
#' @title Convert USGS Hydrologic Unit Shapefile into smaller subsets
#' @param nameOnly logical specifying whether to only return the name without creating the file
#' @description Using the complete WBD dataset downloaded from ftp://rockyftp.cr.usgs.gov/vdelivery/Datasets/Staged/WBD/Shape/
#' and simplified using mapshaper, this function subsets the complete set of polygons into smaller groupings based 
#' on larger Hydrologic Units. Files are saved into a new folder in the data directory as .RData SpatialPolygonsDataFrame. 
#' The data directory can be set using \code{setSpatialDataDir()}. 
#' @return Name of the dataset being created.
#' @references \url{ftp://rockyftp.cr.usgs.gov/vdelivery/Datasets/Staged/WBD/Shape}
#' @seealso setSpatialDataDir

# This data was downloaded as a large zip file for all HUC levels from 
# ftp://rockyftp.cr.usgs.gov/vdelivery/Datasets/Staged/WBD/Shape/. It is not available as an 
# automated download because the file is too large. 

# Spatial Data Directory must be set for this function to work. 






convertUSGSHUC8 <- function(nameOnly=FALSE) {
  
  # Use package internal data directory
  dataDir <- getSpatialDataDir()
  
  # Specify the name of the dataset and file being created
  # TO DO: Change this to change with the level and sublevel of HUC created, to be the appropriate
  # file name. 
  datasetName <- 'USGSHUC6'
  
  if (nameOnly) return(datasetName)

  # Convert shapefile into SpatialPolygonsDataFrame
  dsnPath <- 'WBDNational'
  shpName <- 'WBDHU8-ms'
  SPDF <- convertLayer(dsn=dsnPath,layerName=shpName)

  # Rationalize naming:
  # * human readable full nouns with descriptive prefixes
  # * generally lowerCamelCase
  # with internal standards:
  # * countryCode (ISO 3166-1 alpha-2)
  # * stateCode (ISO 3166-2 alpha-2)
  # * longitude (decimal degrees E)
  # * latitude (decimal degrees N)
  
  #   names(SPDF)
  #    [1] "SHAPE_AREA" "SOURCEFEAT" "AREASQKM"   "METASOURCE" "SOURCEORIG" "HUC8"       "LOADDATE"   "TNMID"     
  #    [9] "AREAACRES"  "GNIS_ID"    "NAME"       "SOURCEDATA" "STATES"     "SHAPE_LENG"
  # Subset this dataframe to include only obviously useful columns
  usefulColumns <- c('AREASQKM', 'HUC8', 'NAME', 'STATES')
  
  SPDF <- SPDF[,usefulColumns]
  names(SPDF) <- c('area','HUC','HUCName', 'stateCode')

  # Change are from km^2 to m^2
  SPDF@data$area <- SPDF@data$area * 1000000
  
  # Group polygons with duplicated hydrologic unit codes
  SPDF <- organizePolygons(SPDF, uniqueID='HUC', sumColumns='area')

  
  
  # Calculate centroids to help add more metadata
  centroids <- rgeos::gCentroid(SPDF, byid=TRUE)
  lon <- sp::coordinates(centroids)[,1]
  lat <- sp::coordinates(centroids)[,2]
  
  # Add more standard columns
  SPDF$longitude <- lon
  SPDF$latitude <- lat  
  SPDF$countryCode <- 'US'
  SPDF$countryName <- 'United States'
  suppressWarnings(SPDF$stateCode <- getStateCode(lon, lat, countryCodes=c('US')))
  SPDF$stateName <- codeToState(SPDF$stateCode, SPDF$countryCode)
   
  # Assign a name and save the data
  assign(datasetName,SPDF)
  
  # QUESTION: why does save have a list command and c(datasetName) in it? 
  save(list=c(datasetName),file=paste0(dataDir,"/",datasetName, '.RData'))
  
  # Break into chunks based on HUC4
  HUC4codes <- substr(SPDF@data$HUC, start = 0, stop =4)
  HUC4codes <- sort(unique(HUC4codes))
  
  dir.create(paste0(dataDir, '/HUC8sByUniqueHUC4'))
  for (i in 1:length(HUC4codes)) {
    print(HUC4codes[[i]])
    regex <- paste0('^', HUC4codes[[i]])
    HUC4 <- SPDF[stringr::str_detect(SPDF@data$HUC, regex),]
    # Save as HUC8_'HUC4code'
    fileName <- paste0('HUC8_', HUC4codes[[i]])
    save(HUC4, file=paste0(dataDir, '/', 'HUC8sByUniqueHUC4/', fileName, '.RData'))
  }
  
  
  
  return(invisible(datasetName))
}

