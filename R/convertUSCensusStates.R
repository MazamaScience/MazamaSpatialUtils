#' @keywords datagen
#' @export
#' @title Convert US Census State Shapefile
#' @param nameOnly logical specifying whether to only return the name without creating the file
#' @description Returns a SpatialPolygonsDataFrame for US States
#' @details A US state shapefile is downloaded and converted to a
#' SpatialPolygonsDataFrame with additional columns of data. The resulting file will be created
#' in the spatial data directory which is set with \code{setSpatialDataDir()}.
#' @return Name of the dataset being created.
#' @references \url{https://www.census.gov/geo/maps-data/data/cbf/cbf_state.html}
#' @seealso setSpatialDataDir
convertUSCensusStates <- function(nameOnly=FALSE) {
  
  # Use package internal data directory
  dataDir <- getSpatialDataDir()
  
  # Specify the name of the dataset and file being created
  datasetName <- 'USCensusStates'
  
  if (nameOnly) return(datasetName)
  
  # Build appropriate request URL for US Census Sates data
  url <- 'http://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_state_500k.zip'
  
  filePath <- paste(dataDir,basename(url),sep='/')
  utils::download.file(url,filePath)
  # NOTE:  This zip file has no directory so extra subdirectory needs to be created
  utils::unzip(filePath,exdir=paste0(dataDir,'/states'))
  
  # Convert shapefile into SpatialPolygonsDataFrame
  # NOTE:  The 'states' directory has been created
  dsnPath <- paste(dataDir,'states',sep='/')
  shpName <- 'cb_2016_us_state_500k'
  SPDF <- convertLayer(dsn=dsnPath,layerName=shpName)
  
  # Rationalize naming:
  # * human readable full nouns with descriptive prefixes
  # * generally lowerCamelCase
  # with internal standards:
  # * countryCode (ISO 3166-1 alpha-2)
  # * stateCode (ISO 3166-2 alpha-2)
  # * longitude (decimal degrees E)
  # * latitude (decimal degrees N)
  # * area (m^2)
  
  # Relabel and standardize the naming in the SpatialPolygonsDataFrame
  
  # NOTE:  STATENS is the Geographic Names Information System name for the state polygon
  
  names(SPDF) <- c('FIPS','GNISCode','AFFGeoID','GeoID','stateCode',
                   'stateName','LSAD','areaLand','areaWater')
  
  # Add countryCode to adhere to the package internal standards
  SPDF$countryCode <- 'US'
  
  # Guarantee that ALAND and AWATER are numeric
  # NOTE:  Areas are already in m^2
  SPDF$areaLand <- as.numeric(SPDF$areaLand)
  SPDF$areaWater <- as.numeric(SPDF$areaWater)
  
  # Group polygons with the same identifier (countryCode)
  SPDF <- organizePolygons(SPDF, uniqueID='stateCode', sumColumns=c('areaLand', 'areaWater'))

  # Assign a name and save the data
  assign(datasetName,SPDF)
  save(list=c(datasetName),file=paste0(dataDir,'/',datasetName,'.RData'))
  
  # Clean up
  unlink(filePath, force=TRUE)
  unlink(dsnPath, recursive=TRUE, force=TRUE)
  
  return(invisible(datasetName))
}

