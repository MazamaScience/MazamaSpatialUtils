#' @keywords datagen
#' @export
#' @title Convert Public Health Districts Shapefile
#' @param nameOnly logical specifying whether to only return the name without creating the file
#' @description Returns a SpatialPolygonsDataFrame for Public Health Districts for Washington, 
#' Oregon, Idaho, and California.
#' @details A Public Health Districts shapefile is downloaded and converted to a 
#' SpatialPolygonsDataFrame with additional columns of data. The resulting file will be created
#' in the spatial data directory which is set with \code{setSpatialDataDir()}.
#' @return Name of the dataset being created.
#' @references \url{http://mazamascience.com/Shapefiles/PHDs.tgz}
#' @seealso setSpatialDataDir
convertPHDs <- function(nameOnly=FALSE) {
  
  # Use package internal data directory
  dataDir <- getSpatialDataDir()
  
  # Specify the name of the dataset and file being created
  datasetName <- 'PHDs'
  
  if (nameOnly) return(datasetName)
  
  # Build appropriate request URL for US County Borders data
  url <- 'http://mazamascience.com/Shapefiles/PHDs.tgz'
  
  filePath <- file.path(dataDir,basename(url))
  utils::download.file(url,filePath)
  # NOTE:  This zip file has no directory so extra subdirectory needs to be created
  utils::untar(filePath, exdir=dataDir)
  
  # Convert shapefile into SpatialPolygonsDataFrame
  # NOTE:  The 'counties' directory has been created
  dsnPath <- file.path(dataDir,'PHDs')
  shpName <- 'LocalPHDs'
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

  # Given row of PHD data, find state code, name, or adm1_code
  extractState <- function(row) {
    fips <- row['stateFIPS']
    stateCode <- MazamaSpatialUtils::US_stateCodes$stateCode[MazamaSpatialUtils::US_stateCodes$fips==paste0("US", fips)]
    return(stateCode)
  }
  
  # Standardize naming in the SpatialPolygonsDataFrame

  
  SPDF@data <- dplyr::select(SPDF@data, 
                             PHDName = .data$NAME,
                             contact = .data$CONTACT,
                             stateFIPS = .data$STATEID) 
  SPDF$countryCode <- 'US'
  SPDF$stateCode <- apply(SPDF@data, 1, extractState)
  
  # Assign a name and save the data
  assign(datasetName,SPDF)
  save(list=c(datasetName),file=paste0(dataDir,'/',datasetName,'.RData'))
  
  # Clean up
  unlink(filePath, force=TRUE)
  unlink(dsnPath, recursive=TRUE, force=TRUE)
  
  return(invisible(datasetName))
}

