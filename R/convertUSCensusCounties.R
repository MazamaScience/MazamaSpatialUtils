#' @keywords datagen
#' @export
#' @title Convert US County Borders Shapefile
#' @param nameOnly logical specifying whether to only return the name without creating the file
#' @description Returns a SpatialPolygonsDataFrame for a US county divisions
#' @details A US county borders shapefile is downloaded and converted to a 
#' SpatialPolygonsDataFrame with additional columns of data. The resulting file will be created
#' in the spatial data directory which is set with \code{setSpatialDataDir()}.
#' @return Name of the dataset being created.
#' @references \url{http://www2.census.gov/geo/tiger/GENZ2013}
#' @seealso setSpatialDataDir
#' @seealso getUSCounty
convertUSCensusCounties <- function(nameOnly=FALSE) {
  
  # Use package internal data directory
  dataDir <- getSpatialDataDir()
  
  # Specify the name of the dataset and file being created
  datasetName <- 'USCensusCounties'
    
  if (nameOnly) return(datasetName)

  # Build appropriate request URL for US County Borders data
  url <- 'http://www2.census.gov/geo/tiger/GENZ2013/cb_2013_us_county_20m.zip'
  
  filePath <- file.path(dataDir,basename(url))
  utils::download.file(url,filePath)
  # NOTE:  This zip file has no directory so extra subdirectory needs to be created
  utils::unzip(filePath,exdir=file.path(dataDir,'counties'))
  
  # Convert shapefile into SpatialPolygonsDataFrame
  # NOTE:  The 'counties' directory has been created
  dsnPath <- file.path(dataDir,'counties')
  shpName <- 'cb_2013_us_county_20m'
  SPDF <- convertLayer(dsn=dsnPath, layerName=shpName, encoding='latin1')
  
  # Rationalize naming:
  # * human readable full nouns with descriptive prefixes
  # * generally lowerCamelCase
  # with internal standards:
  # * countryCode (ISO 3166-1 alpha-2)
  # * stateCode (ISO 3166-2 alpha-2)
  # * longitude (decimal degrees E)
  # * latitude (decimal degrees N)
  # * area (m^2)
  
 
  # Given row of USCensusCounties data, find state code, name, or adm1_code
  extractState <- function(row) {
    fips <- row['stateFIPS']
    stateCode <- MazamaSpatialUtils::US_stateCodes$stateCode[MazamaSpatialUtils::US_stateCodes$fips==paste0("US", fips)]
    return(stateCode)
  }
  
  # Standardize naming in the SpatialPolygonsDataFrame
  
  # TODO:  Figure out units for ALAND and AWATER and convert to m^2
  
  # Guarantee that ALAND and AWATER are numeric
  SPDF$ALAND <- as.numeric(SPDF$ALAND)
  SPDF$AWATER <- as.numeric(SPDF$AWATER)
  
  SPDF@data <- dplyr::select(.data = SPDF@data, 
                             countyFIPS = .data$COUNTYFP,
                             areaLand = .data$ALAND,
                             areaWater = .data$AWATER,
                             countyName = .data$NAME,
                             stateFIPS = .data$STATEFP,
                             COUNTYNS = .data$COUNTYNS) 
  SPDF$stateCode <- apply(SPDF@data, 1, extractState)
  SPDF$countryCode <- "US"
  SPDF$name <- SPDF$countyName
  
  # TODO:  COUNTYNS is the polygon uniqueID but what is it?
  
  # Group polygons with the same identifier (countyName)
  SPDF <- organizePolygons(SPDF, uniqueID='COUNTYNS', sumColumns=c('areaLand','areaWater'))
  
  # Assign a name and save the data
  assign(datasetName,SPDF)
  save(list=c(datasetName),file=paste0(dataDir,'/',datasetName,'.RData'))
  
  # Clean up
  unlink(filePath, force=TRUE)
  unlink(dsnPath, recursive=TRUE, force=TRUE)
  
  return(invisible(datasetName))
}

