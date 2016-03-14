#' @keywords datagen
#' @export
#' @title Convert NOAA Hazard Mapping System Smoke Shapefiles
#' @param dsnPath directory where the HMS Smoke datasets are found
#' @param datestamp HMS datestamp in the format "YYYYmmdd"
#' @param nameOnly logical specifying whether to only return the name without creating the file
#' @description Previously downloaded smoke shapefiles from the NOAA 
#' \href{http://www.ospo.noaa.gov/Products/land/hms.html}{Hazard Mapping System} are converted to a 
#' SpatialPolygonsDataFrame with additional columns of data. The resulting file will be
#' created in the spatial data directory which is set with \code{setSpatialDataDir()}.
#' @details The full WBD dataset can be downloaded from the USGS with the 
#' following command:
#' \preformatted{
#' wget ftp://satepsanone.nesdis.noaa.gov/FIRE/HMS/GIS/ARCHIVE/hms_smoke*
#' }
#' 
#' @return Name of the dataset being created.
#' @references \url{http://www.ospo.noaa.gov/Products/land/hms.html}
#' @seealso setSpatialDataDir

# TODO:  Convert missing state codes to state codes from allStateCode, with a note explaining 
# TODO:  how and why. Figure out why it is printing all those numbers when it runs and change.  

convertWBDHUC <- function(dsnPath=NULL, datestamp=strftime(Sys.Date(),"%Y%m%d","UTC"), 
                          nameOnly=FALSE) {
  
  # Sanity check dsnPath
  if ( is.null(dsnPath) ) stop(paste0('Argument dsnPath must be specified.'))
  if ( !file.exists(dsnPath) ) stop(paste0('dsnPath="',dsnPath,'" not found.'))
  
  # Use package internal data directory
  dataDir <- getSpatialDataDir()
  
  # Specify the name of the dataset and file being created
  datasetName <- paste0('HMSSmoke_', datestamp) 
  
  if (nameOnly) return(datasetName)

  # Convert shapefile into SpatialPolygonsDataFrame
  layerName <- paste0('hms_smoke', datestamp)
  SPDF <- convertLayer(dsn=dsnPath, layerName=layerName)

  # Calculate centroids to help add more metadata
  result <- try( {
    centroids <- rgeos::gCentroid(SPDF, byid=TRUE)
    lon <- sp::coordinates(centroids)[,1]
    lat <- sp::coordinates(centroids)[,2]
  }, silent=TRUE)
  
  # NOTE:  If centroids don't work we'll just default to the center of the bbox for each polygon
  
  if ( class(result)[1] == "try-error" ) {
    cat(paste0('NOTE: rgeos::gCentroid() failed with the following message. Using bbox() to calculate lon and lat.\n'))
    cat(paste0(geterrmessage(),'\n'))
    lon <- rep(as.numeric(NA), nrow(SPDF))
    lat <- rep(as.numeric(NA), nrow(SPDF))
    for (i in 1:nrow(SPDF)) {
      bbox <- bbox(SPDF[i,])
      lon[i] <- mean(bbox[1,])
      lat[i] <- mean(bbox[2,])
    }
  }
  
  # Add standard columns
  SPDF$longitude <- lon
  SPDF$latitude <- lat  
  SPDF$countryCode <- getCountryCode(lon, lat, useBuffering=TRUE)
  SPDF$stateCode <- getStateCode(lon, lat, countryCodes=unique(SPDF$countryCode), useBuffering=TRUE)
  SPDF$timezone <- getTimezone(lon, lat, useBuffering=TRUE)
  
  # Add POSIXct times to dataframe
  SPDF@data$starttime <- lubridate::ymd_hm( paste0(datestamp,SPDF@data$Start) )
  SPDF@data$endtime <- lubridate::ymd_hm( paste0(datestamp,SPDF@data$End) )
  
  # Add numeric density to dataframe
  SPDF@data$density <- as.numeric(SPDF@data$Density)
  
  # Retain useful columns
  SPDF <- SPDF[,c('starttime','endtime','density','longitude','latitude','countryCode','stateCode','timezone')]
  

  # Assign a name and save the data
  assign(datasetName,SPDF)
  save(list=c(datasetName),file=paste0(dataDir,"/",datasetName, '.RData'))
  
  return(invisible(datasetName))
}

