#' @keywords datagen
#' @export
#' @title Convert Exclusive Economic Zones Countries Shapefile
#' @param nameOnly logical specifying whether to only return the name without creating the file
#' @param dsnPath directory where EEZCountries .zip file is found
#' @description Previously downloaded file from \url{http://www.marineregions.org/downloads.php#unioneezcountry} 
#' is converted to a SpatialPolygonsDataFrame with additional columns of data. The resulting
#' file will be created in the spatial data directory which is set with \code{setSpatialDataDir()}.
#' @details  The dataset can be downloaded from 
#' \url{http://www.marineregions.org/download_file.php?name=EEZ_land_union_v2_201410.zip}
#' by answering the questions and clicking "download".  
#' @return Name of the dataset being created.
#' @references \url{http://www.marineregions.org/downloads.php#unioneezcountry}
#' @references VLIZ (2014). Union of the ESRI Country shapefile and the Exclusive 
#' Economic Zones (version 2). Available online at http://www.marineregions.org/. Consulted on 2017-07-20.
#' @seealso setSpatialDataDir
#' @seealso getCountry, getCountryCode
convertEEZCountries <- function(dsnPath=NULL, nameOnly=FALSE) {
  
  # Sanity check dsnPath
  if ( is.null(dsnPath) ) stop(paste0('Argument dsnPath must be specified. dsnPath indicates the file path for the .zip file which can be downloaded from http://www.marineregions.org/download_file.php?name=EEZ_land_union_v2_201410.zip'))
  if ( !file.exists(dsnPath) ) stop(paste0('dsnPath="',dsnPath,' not found. dsnPath indicates the file path for the .zip file which can be downloaded from http://www.marineregions.org/download_file.php?name=EEZ_land_union_v2_201410.zip'))
  
  # Use package internal data directory
  dataDir <- getSpatialDataDir()
  
  # Specify the name of the file being created
  datasetName <- 'EEZCountries'
  
  if (nameOnly) return(datasetName)
  
  # Unzip the downloaded file
  utils::unzip(dsnPath,exdir=paste0(dataDir, '/', "EEZ"))
  dsnPath <- paste(dataDir, 'EEZ', sep='/')
  
  # Convert shapefile into SpatialPolygonsDataFrame
  # NOTE:  The 'EEZCountries' directory has been created
  # NOTE:  Simplify the .shp file using Mapshaper prior to converting layer
  SPDF <- convertLayer(dsn=dsnPath,layerName='EEZ_land_v2_201410')
  
  # > names(SPDF)
  # [1] "OBJECTID"   "ISO_3digit" "Country"    "Changes"    "Shape_Leng" "Shape_Area"
  
  # Rationalize naming:
  # * human readable full nouns with descriptive prefixes
  # * generally lowerCamelCase
  # with internal standards:
  # * countryCode (ISO 3166-1 alpha-2)
  # * stateCode (ISO 3166-2 alpha-2)
  # * longitude (decimal degrees E)
  # * latitude (decimal degrees N)
  # * area (m^2)
  usefulColumns <- c('OBJECTID', 'ISO_3digit', 'Country', 'Changes')
  SPDF <- SPDF[,usefulColumns]
  names(SPDF) <- c('objectID', 'ISO3', 'countryName', 'changes')
  
  # Change missing countryCodes to NA
  SPDF$ISO3[SPDF$ISO3 == '-' ] <- NA
  

  # > SPDF@data[which(stringr::str_length(SPDF$ISO3) != 3),]
  # objectID  ISO3                    countryName changes     area
  # 65        66    CW                        Curaçao    <NA>  2.57573
  # 152      154 MNP++ Northern Marinana Islands-Guam    <NA> 82.66514
  SPDF@data["65","ISO3"] <- "CUW"
  SPDF@data["152","ISO3"] <- "GUM"
  
  # Add more standard columns
  SPDF$countryCode <- iso3ToIso2(SPDF$ISO3)
  
  SPDF <- organizePolygons(SPDF, uniqueID='objectID')
  
  # Assign a name and save the data for World EEZ
  assign(datasetName,SPDF)
  save(list=c(datasetName),file=paste0(dataDir,'/',datasetName,'.RData'))
  
  return(invisible(datasetName))
}

