#' @keywords datagen
#' @title Convert (Simple) World Borders Shapefile
#' @param nameOnly logical specifying whether to only return the name without creating the file
#' @param dsnPath directory where EEZCountries .zip file is found
#' @description Returns a SpatialPolygonsDataFrame for a simple world divisions
#' @details A previously downloaded world borders shapefile is converted to a
#' SpatialPolygonsDataFrame with additional columns of data. The resulting file will be saved in
#' the data/ directory. The dataset can be downloaded from 
#' \url{http://www.marineregions.org/download_file.php?name=EEZ_land_union_v2_201410.zip}
#' by answering the questions and clicking "download".  
#'
#' The SimpleCountriesEEZ shapefile is the same as the EEZCountries shapefile. Polygons for coastal countries include 
#' a 200 mile buffer, corresponding to their Exclusive Economic Zones, so this shapefile is especially suited
#' for spatial searches. This is the default dataset used in \code{getCountry()} and \code{getCountryCode()}.
#' Users may wish to use a higher resolution dataset when plotting.
#' @note This is a non-exported function used only for updating the package dataset.
#' @return Name of the dataset being created.
#' @references \url{http://www.marineregions.org/downloads.php}
#' @seealso setSpatialDataDir
#' @seealso getCountry, getCountryCode
#' 
convertSimpleCountriesEEZ <- function(dsnPath = NULL, nameOnly=FALSE) {

  # NOTE:  This function only needs to be run once to update the package SimpleCountriesEEZ
  # NOTE:  dataset. It is included here for reproducibility.
  
  # NOTE:  This function should be run wile working with the package source code.
  # NOTE:  The working directory should be MazamaSpatialUtils/ and the resulting
  # NOTE:  .RData file will be kept in the data/ directory
  
  # Sanity check dsnPath
  if ( is.null(dsnPath) ) stop(paste0('Argument dsnPath must be specified. dsnPath indicates the file path for the .zip file downloaded from http://www.marineregions.org/download_file.php?name=EEZ_land_union_v2_201410.zip'))
  if ( !file.exists(dsnPath) ) stop(paste0('dsnPath="',dsnPath,' not found. dsnPath indicates the file path for the .zip file downloaded from http://www.marineregions.org/download_file.php?name=EEZ_land_union_v2_201410.zip'))
  
  sourceCodeDir <- getwd()
  
  # Use package internal data directory
  dataDir <- getSpatialDataDir()
  
  # Specify the name of the dataset and file being created
  datasetName <- 'SimpleCountriesEEZ'
  
  if (nameOnly) return(datasetName)
  
  # Unzip the downloaded file into a new folder, EEZ
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
  
  # Assign a name and save the data
  assign(datasetName,SPDF)
  save(list=c(datasetName),file=paste0(sourceCodeDir,'/data/',datasetName,'.RData'))
  
  # Clean up
  unlink(dsnPath, recursive=TRUE, force=TRUE)
  
  return(invisible(datasetName))
}