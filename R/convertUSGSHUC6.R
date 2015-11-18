#' @keywords datagen
#' @export
#' @title Convert USGS Hydrologic Unit Shapefile
#' @param nameOnly logical specifying whether to only return the name without creating the file
#' @description A hydrologic unit shapefile is downloaded and converted to a 
#' SpatialPolygonsDataFrame. The resulting file will be created in the data directory 
#' which can be set with \code{setSpatialDataDir()}.
#' @details The HUC250k dataset consists of 8-digit HUCs.
#' @return Name of the dataset being created.
#' @references \url{http://water.usgs.gov/GIS/metadata/usgswrd/XML/huc250k.xml}
#' @seealso setSpatialDataDir

# This data was downloaded as a large zip file for all HUC levels from 
# ftp://rockyftp.cr.usgs.gov/vdelivery/Datasets/Staged/WBD/Shape/. It is not available as an 
# automated download because the file is too large. 

# Spatial Data Directory must be set for this function to work. 


dsnPath <- 'WBDNational'
shpName <- 'WBDHU6'

# Specify the name of the dataset and file being created
datasetName <- 'USGSHUC6'


convertUSGSHUC <- function(dsnPath, shpNames, datasetName, nameOnly=FALSE) {
  
  # Use package internal data directory
  dataDir <- getSpatialDataDir()
  
 
  
  if (nameOnly) return(datasetName)

  # Convert shapefile into SpatialPolygonsDataFrame
  
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
  #    [1] "SHAPE_AREA" "SOURCEFEAT" "AREASQKM"   "METASOURCE" "HUC6"       "SOURCEORIG" "LOADDATE"   "TNMID"     
  #    [9] "GEODB_OID"  "OBJECTID"   "AREAACRES"  "GNIS_ID"    "NAME"       "SOURCEDATA" "STATES"     "SHAPE_LENG"       
  
  # Subset this dataframe to include only obviously useful columns
  usefulColumns <- c('AREASQKM', 'HUC6', 'NAME', 'STATES')
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
  # NOTE: Some HUCs already have single state names, importing these where existing.
  if (stringr::str_length(SPDF@data$stateCode >=3)) {
    SPDF@data$stateCode <- ''
  } else {
    SPDF@daata$stateCode <- SPDF@data$stateCode
  }
#   if (stringr::str_length(SPDF@data$stateCode)==2) {
#     stateCode <- stateCode
#   } 
#   else {
#     suppressWarnings(SPDF$stateCode <- getStateCode(lon, lat, countryCodes=c('US')))
#   }
  
  # TODO: write if statement for HUCS that have more than one state code to use centroid to find state. 
 
  SPDF$stateName <- codeToState(SPDF$stateCode, SPDF$countryCode)
   
  # Assign a name and save the data
  assign(datasetName,SPDF)
  save(list=c(datasetName),file=paste0(dataDir,"/",datasetName, '.RData'))
  
  # Clean up
  unlink(filePath, force=TRUE)
  unlink(dsnPath, recursive=TRUE, force=TRUE)
  
  return(invisible(datasetName))
}

