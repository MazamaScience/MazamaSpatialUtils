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
#' @details The full set of archived HMS Smoke shapefiles can be downloaded from NOAA with the
#' following command:
#'
#' \preformatted{
#' wget -R '.zip' ftp://satepsanone.nesdis.noaa.gov/FIRE/HMS/GIS/ARCHIVE/hms_smoke*
#' }
#'
#' If no \code{datestamp} argument is used, all shapefiles in \code{dsnPath} will be converted.
#' In this case, a vector of created dataset names is returned.
#' @note Data files prior to August 13, 2007 do not contain the vital 'Density' column. For these
#' files, \code{NA} will be used in the converted dataframes.
#' @return Name of the dataset being created.
#' @references \url{http://www.ospo.noaa.gov/Products/land/hms.html}
#' @seealso setSpatialDataDir

convertHMSSmoke <- function(dsnPath=NULL, datestamp=NULL, nameOnly=FALSE) {

  # Sanity check dsnPath
  if ( is.null(dsnPath) ) stop(paste0('Argument dsnPath must be specified.'))
  if ( !file.exists(dsnPath) ) stop(paste0('dsnPath="',dsnPath,'" not found.'))

  # Use package internal data directory
  dataDir <- getSpatialDataDir()

  if ( is.null(datestamp) ) {

    # Create a list of datestamps from files in the directory and call this function
    # recursively with Recall(...).
    shapefiles <- list.files(dsnPath,pattern='hms_smoke.*\\.shp')
    datestamps <- stringr::str_sub(shapefiles,10,17)
    #TODO: better acknowledgement of what datestamp is
    for ( datestamp in datestamps ) {
      result <- try( Recall(dsnPath, datestamp), # 'Recall' is a placedholder for the name of the function in which it is called.
                     silent=TRUE)
      if ( class(result)[1] == "try-error" ) {
        warning(geterrmessage(),'\n')
      }
    }
    datasetNames <- paste0('HMSSmoke_',datestamps)
    return(invisible(datasetNames))

  } else {

    # Specify the name of the dataset and file being created
    datasetName <- paste0('HMSSmoke_', datestamp)
    if (nameOnly) return(datasetName)

  }


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
    warning('NOTE: rgeos::gCentroid() failed with the following message. Using bbox() to calculate lon and lat.\n')
    warning(geterrmessage(),'\n')
    lon <- rep(as.numeric(NA), nrow(SPDF))
    lat <- rep(as.numeric(NA), nrow(SPDF))
    for (i in seq_len(nrow(SPDF)) ) {
      bbox <- sp::bbox(SPDF[i,])
      lon[i] <- mean(bbox[1,])
      lat[i] <- mean(bbox[2,])
    }
  }

  # Add standard columns
  SPDF$longitude <- lon
  SPDF$latitude <- lat
  SPDF$countryCode <- getCountryCode(lon, lat, useBuffering=TRUE)
  if ( !exists('NaturalEarthAdm1') ) {
    loadSpatialData('NaturalEarthAdm1')
  }
  SPDF$stateCode <- getStateCode(lon, lat, countryCodes=unique(SPDF$countryCode), useBuffering=TRUE)
  SPDF$timezone <- getTimezone(lon, lat, useBuffering=TRUE)

  # Add POSIXct times to dataframe
  SPDF$starttime <- lubridate::ymd_hm( paste0(datestamp, stringr::str_split_fixed(SPDF$Start, " ", 2)[, 2] ))
  SPDF$endtime <- lubridate::ymd_hm( paste0(datestamp, stringr::str_split_fixed(SPDF$End, " ", 2)[, 2] ))

  # Add numeric density to dataframe
  # NOTE:  data files prior to August 13, 2007 are missing the 'Density' column
  if ('Density' %in% names(SPDF)) {
    SPDF$density <- as.numeric(SPDF@data$Density)
  } else {
    SPDF$density <- as.numeric(NA)
  }

  # Retain useful columns
  SPDF <- SPDF[,c('starttime','endtime','density','longitude',
                  'latitude','countryCode','stateCode','timezone')]

  # Assign a name and save the data
  assign(datasetName,SPDF)
  save(list=c(datasetName),file=paste0(dataDir,"/",datasetName, '.RData'))

  return(invisible(datasetName))
}

