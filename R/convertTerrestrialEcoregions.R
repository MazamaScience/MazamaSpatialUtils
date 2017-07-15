#' @keywords datagen
#' @export
#' @title Create Terrestrial Ecosystem dataset
#' @param nameOnly logical specifying whether to only return the name without creating the file
#' @description A shapefile is downloaded from \url{https://www.worldwildlife.org/publications/terrestrial-ecoregions-of-the-world}
#' and converted to a SpatialPolygonsDataFrame with additional columns of data. The resulting file will be created
#' in the spatial data directory which is set with \code{setSpatialDataDir()} 
#' @return Name of the dataset being created.
#' @seealso setSpatialDataDir

convertTerrestrialEcoregions <- function(nameOnly=FALSE) {
  
  # Use package internal data directory
  dataDir <- getSpatialDataDir()
  
  # Specify the name of the file being created
  datasetName <- 'OSMTimezones'
  
  if (nameOnly) return(datasetName)
  
  # Build appropriate request URL for terrestrial ecoregions
  url <- "https://c402277.ssl.cf1.rackcdn.com/publications/15/files/original/official_teow.zip?1349272619"
  
  filePath <- paste(dataDir,basename(url),sep='/')
  utils::download.file(url,filePath)
  utils::unzip(filePath,exdir=dataDir)
  
  # Convert shapefile into SpatialPolygonsDataFrame
  dsnPath <- paste(dataDir,'official',sep='/')
  SPDF <- convertLayer(dsn=dsnPath,layerName='wwf_terr_ecos')
  
  #########################################################################################################
  # Everything below here is for timezones and needs to be updated
  
  
  # Rename "TZID" to "timezone"
  names(SPDF@data) <- c('timezone')
  
  # Now get additional data from Wikipedia
  wikipediaTimezoneTable <- convertWikipediaTimezoneTable()
  
  # Merge the additional data onto the @data slot of the SPDF
  SPDF@data <- dplyr::left_join(SPDF@data, wikipediaTimezoneTable, by="timezone")
  
  # Group polygons with the same identifier
  SPDF <- organizePolygons(SPDF, uniqueID='timezone')
  
  # Create a simplified version at 5%
  SPDF_05 <- rmapshaper::ms_simplify(SPDF, .05)
  SPDF_05@data$rmapshaperid <- NULL
  datasetName_05 <- paste0(datasetName, "_05")
  
  # Assign a name and save the data
  assign(datasetName,SPDF)
  assign(datasetName_02, SPDF_05)
  save(list=c(datasetName),file=paste0(dataDir,'/',datasetName,'.RData'))
  save(list=c(datasetName_05), file=paste0(dataDir,'/',datasetName_05,'.RData'))
  
  # Clean up
  unlink(filePath, force=TRUE)
  unlink(dsnPath, recursive=TRUE, force=TRUE)
  
  return(invisible(datasetName))
}

