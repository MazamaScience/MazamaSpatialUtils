#' @keywords datagen
#' @export
#' @title Convert World Exclusive Economic Zones Boundaries Shapefile
#' @param nameOnly logical specifying whether to only return the name without creating the file
#' @description A world EEZ shapefile is downloaded and converted to a 
#' SpatialPolygonsDataFrame with additional columns of data. The resulting file will be created
#' in the spatial data directory which is set with \code{setSpatialDataDir()}.
#' @return Name of the dataset being created.
#' @references \url{http://www.marineregions.org/downloads.php}
#' @seealso setSpatialDataDir
#' @seealso getCountry, getCountryCode
convertWorldEEZ <- function(nameOnly=FALSE) {
  
  # ----- Setup ----------------------------------------------------------------
  
  # Use package internal data directory
  dataDir <- getSpatialDataDir()
  
  # Specify the name of the file being created
  datasetName <- 'WorldEEZ'
  
  if (nameOnly) return(datasetName)
  
  # Test if the shapefile directory exists.
  if (!file.exists(paste0(dataDir,'/',datasetName))) {
    stop('Shapefile directory does not exists. Please download and convert the shapefile desired.', call.=FALSE)
  } 
  
  # ----- Get the data ---------------------------------------------------------
  
  # Unzip the downloaded file
  filePath <- file.path(dataDir,'World_EEZ_v11_20191118_LR.zip')
  utils::unzip(filePath,exdir=file.path(dataDir))
  dsnPath <- file.path(dataDir,'World_EEZ_v11_20191118_LR')

  # ----- Convert to SPDF ------------------------------------------------------
  
  # Convert shapefile into SpatialPolygonsDataFrame
  # NOTE:  The 'WorldEEZ' directory has been created
  # NOTE:  Simplify the .shp file using Mapshaper prior to converting layer
  SPDF <- convertLayer(dsn=dsnPath,layerName='eez_v11_lowres')
  
  
  
  #   WIP: All the column names don't match. Either I'm doing something wrong
  #   or they have changed some things around. 
  #   NEW
  #   > names(SPDF)
  #   [1] "MRGID"      "GEONAME"    "MRGID_TER1" "POL_TYPE"   "MRGID_SOV1" "TERRITORY1" "ISO_TER1"  
  #   [8] "SOVEREIGN1" "MRGID_TER2" "MRGID_SOV2" "TERRITORY2" "ISO_TER2"   "SOVEREIGN2" "MRGID_TER3"
  #   [15] "MRGID_SOV3" "TERRITORY3" "ISO_TER3"   "SOVEREIGN3" "X_1"        "Y_1"        "MRGID_EEZ" 
  #   [22] "AREA_KM2"   "ISO_SOV1"   "ISO_SOV2"   "ISO_SOV3"   "UN_SOV1"    "UN_SOV2"    "UN_SOV3"   
  #   [29] "UN_TER1"    "UN_TER2"    "UN_TER3"   
  
  
  #   OLD
  #   > names(SPDF)
  #   [1] "OBJECTID"    "EEZ"        "Country"    "ID"        "Sovereign"  "Remarks"    "Sov_ID"     "EEZ_ID"     "ISO_3digit"  "MRGID"
  #   [11] "Date_chang" "Area_m2"    "Longitude"  "Latitude"  "MRGID_EEZ"
  
  # ----- Select useful columns and rename -------------------------------------
  
  # Rationalize naming:
  # * human readable full nouns with descriptive prefixes
  # * generally lowerCamelCase
  # with internal standards:
  # * countryCode (ISO 3166-1 alpha-2)
  # * stateCode (ISO 3166-2 alpha-2)
  # * longitude (decimal degrees E)
  # * latitude (decimal degrees N)
  usefulColumns <- c('EEZ', 'Country', 'Sovereign', 'Remarks', 'EEZ_ID', 'ISO_3digit', 'MRGID', 'Area_m2', 'Longitude', 'Latitude')
  SPDF <- SPDF[,usefulColumns]
  names(SPDF) <- c('EEZ', 'country', 'sovereign', 'remarks', 'EEZ_ID', 'ISO3', 'MRGID', 'area', 'longitude', 'latitude')
  
  # Change missing countryCodes to NA
  SPDF$ISO3[SPDF$ISO3 == '-' ] <- NA
  
  # Add more standard columns
  SPDF$countryCode <- iso3ToIso2(SPDF$ISO3)
  SPDF$countryName <- codeToCountry(SPDF$countryCode)
  
  # ----- Organize polygons ----------------------------------------------------
  
  SPDF <- organizePolygons(SPDF, uniqueID='countryCode')
  
  # ----- Simplify -------------------------------------------------------------
  
  
  # ----- Name and save the data -----------------------------------------------
  
  # Assign a name and save the data for World EEZ
  assign(datasetName,SPDF)
  save(list=c(datasetName),file=paste0(dataDir,'/',datasetName,'.RData'))
  
  # ----- Return ---------------------------------------------------------------
  
  return(invisible(datasetName))
}
