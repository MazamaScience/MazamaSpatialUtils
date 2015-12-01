#' @keywords datagen
#' @export
#' @title Convert USGS Hydrologic Unit Shapefile into smaller subsets
#' @param level character string which must be 2, 4, 6, 8, 10 or 14. 12 is not currently supported. . 
#' @param nameOnly logical specifying whether to only return the name without creating the file
#' @description Using the complete WBD dataset downloaded from ftp://rockyftp.cr.usgs.gov/vdelivery/Datasets/Staged/WBD/Shape/
#' and simplified using mapshaper, this function subsets the complete set of polygons into smaller groupings based 
#' on larger Hydrologic Units. Files are saved into a new folder in the data directory as .RData SpatialPolygonsDataFrame. 
#' The data directory can be set using \code{setSpatialDataDir()}. 
#' @note This function requires that the data be downloaded from the URL below and 
#' simplified to a much smaller size using mapshaper software and the command 
#' mapshaper filname.shp -simplify 2% -o 
#' #TODO change this below - what should this function return? 
#' @return Name of the dataset being created.
#' @references \url{ftp://rockyftp.cr.usgs.gov/vdelivery/Datasets/Staged/WBD/Shape}
#' @seealso setSpatialDataDir

# This data was downloaded as a large zip file for all HUC levels from 
# ftp://rockyftp.cr.usgs.gov/vdelivery/Datasets/Staged/WBD/Shape/. It is not available as an 
# automated download because the file is too large. 

# Spatial Data Directory must be set for this function to work. 



#TODO: convert missing state codes to state codes from allStateCode, with a note explaining 
#TODO: how and why. Figure out why it is printing all those numbers when it runs and change.  


convertUSGSHUC <- function(level='8', nameOnly=FALSE) {
  
  # Use package internal data directory
  dataDir <- getSpatialDataDir()
  
  # Specify the name of the dataset and file being created
  datasetName <- paste0('USGSHUC', level) 
  
  if (nameOnly) return(datasetName)

  # Convert shapefile into SpatialPolygonsDataFrame
  dsnPath <- 'WBDNational'
  shpName <- paste0('WBDHU', level, '-ms')
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
  HUC <- paste0('HUC', level)
  usefulColumns <- c('AREASQKM', HUC, 'NAME', 'STATES')
  
  SPDF <- SPDF[,usefulColumns]
  names(SPDF) <- c('area','HUC','HUCName', 'allStateCodes')

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
  save(list=c(datasetName),file=paste0(dataDir,"/",datasetName, '.RData'))
  
  return(invisible(datasetName))
}

