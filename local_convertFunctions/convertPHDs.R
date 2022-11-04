#' @keywords datagen
#' @export
#' @title Convert Public Health Districts Shapefile
#' @param nameOnly logical specifying whether to only return the name without creating the file
#' @description Returns a simple features data frame for Public Health Districts for Washington, 
#' Oregon, Idaho, and California.
#' @details A Public Health Districts shapefile is downloaded and converted to a 
#' simple features data frame with additional columns of data. The resulting file will be created
#' in the spatial data directory which is set with \code{setSpatialDataDir()}.
#' @return Name of the datasetName being created.
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
  
  # Convert shapefile into simple features data frame
  # NOTE:  The 'counties' directory has been created
  dsnPath <- file.path(dataDir,'PHDs')
  shpName <- 'LocalPHDs'
  SFDF <- .convertLayer(dsn=dsnPath, layer=shpName, encoding='latin1')
  
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
    stateFIPS <- row['stateFIPS']
    stateCode <- MazamaSpatialUtils::US_stateCodes$stateCode[MazamaSpatialUtils::US_stateCodes$stateFIPS==paste0("US", stateFIPS)]
    return(stateCode)
  }
  
  # Standardize naming in the simple features data frame

  
  SFDF <- dplyr::select(SFDF, 
                             PHDName = .data$NAME,
                             contact = .data$CONTACT,
                             stateFIPS = .data$STATEID) 
  SFDF$countryCode <- 'US'
  SFDF$stateCode <- apply(SFDF, 1, extractState)
  
  # Assign a name and save the data
  assign(datasetName,SFDF)
  save(list=c(datasetName),file=paste0(dataDir,'/',datasetName,'.RData'))
  
  # Clean up
  unlink(filePath, force=TRUE)
  unlink(dsnPath, recursive=TRUE, force=TRUE)
  
  return(invisible(datasetName))
}

