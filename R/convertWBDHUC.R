#' @keywords datagen
#' @export
#' @title Convert Watershed Boundary Dataset Hydrologic Unit Shapefile
#' @param digit numeric value specifying which digit level huc to be converted -- defaults to 2
#' @param simplified logical specifying whether to use the simplified HUC shapefile
#' @param nameOnly logical specifying whether to only return the name without creating the file
#' @description Returns a SpatialPolygons Data Frame for hydrologic unit divisions
#' @details A hydrologic unit shapefile is converted to a SpatialPolygonsDataFrame. The 
#' resulting file will be created in the package \code{SpatialDataDir} which can be set 
#' with \code{setSpatialDataDir()}.
#' 
#' Original data is downloaded from ftp://ftp.ftw.nrcs.usda.gov/wbd/ as a .gdb file.
#' User must download QGIS to convert .gdb files to shapefiles. The shapefile directory must have 
#' the correct naming convention ('WBD_HUC_2' for 2-digit huc, and so on) and it must be in the 
#' internal data directory set by \code{setSpatialDataDir()}.
#' @return Name of the dataset being created.
#' @references ftp://ftp.ftw.nrcs.usda.gov/wbd/
#' @seealso setSpatialDataDir
convertWBDHUC <- function(digit=2, simplified=TRUE, nameOnly=FALSE) {
  
  # Test if the correct digit huc is valid
  if (!digit %in% seq(2, 12, 2)) {
    stop('Please specify a correct digit. ', digit, '-digit huc does not exist.', call.=FALSE)
  }
  
  # Use package internal data directory
  dataDir <- getSpatialDataDir()
  
  # Specify the name of the dataset and file being created
  datasetName <- paste0('WBD_HUC_', digit)
  
  if (nameOnly) return(datasetName)
  
  # Test if the shapefile directory exists.
  if (!file.exists(paste0(dataDir,'/',datasetName))) {
    stop('Shapefile directory does not exists. Please download and convert the shapefile desired.', call.=FALSE)
  }
  
  # Convert shapefile into SpatialPolygonsDataFrame
  dsnPath <- paste0(dataDir,'/', datasetName)
  if (simplified) {
    shpName <- paste0(datasetName, '-ms')
  } else {
    shpName <- datasetName
  }
  SPDF <- convertLayer(dsn=dsnPath, layerName=shpName)
  
  # Subset this dataframe to include only obviously useful columns
  SPDF <- SPDF[,c(12:8)]
  
  # Standardize naming in the SpatialPolygonsDataFrame
  names(SPDF) <- c('name', paste0('HUC',digit), 'states', 'area', 'area_acres')
  SPDF$countryCode <- 'US'
  SPDF$countryName <- 'United States'
  
  # Rationalize units:
  # * SI  
  # NOTE:  Area seems to be in units of km^2. Convert these to m^2
  SPDF$area <- SPDF$area * 1e6
  
  # Group polygons with the same identifier (hydrologic unit codes)
  SPDF <- organizePolygons(SPDF, uniqueID=paste0('HUC',digit))
  
  # Assign a name and save the data
  assign(datasetName,SPDF)
  save(list=c(datasetName),file=paste0(dataDir,"/",datasetName, '.RData'))
  
  # Clean up
  unlink(dsnPath, recursive=TRUE, force=TRUE)
  
  return(invisible(datasetName))
}

