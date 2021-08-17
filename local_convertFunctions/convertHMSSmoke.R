#' @keywords datagen
#' @importFrom rlang .data
#' @export
#'
#' @title Convert NOAA Hazard Mapping System Smoke Shapefiles
#'
#' @param datestamp UTC datestamp in the format "YYYYMMDD"
#' @param nameOnly logical specifying whether to only return the name without creating the file
#'
#' @description Create a SpatialPolygonsDataFrame for US states
#'
#' @details A NOAA smoke shapefile from the is downloaded and converted to a
#' SpatialPolygonsDataFrame with additional columns of data. The resulting file
#' will be created in the spatial data directory which is set with
#' \code{setSpatialDataDir()}.
#'
#' @note Data files prior to August 13, 2007 do not contain the vital 'Density'
#' column. For these files, \code{NA} will be used in the converted dataframes.
#'
#' @return Name of the dataset being created.
#'
#' @references \url{http://www.ospo.noaa.gov/Products/land/hms.html}
#'
#' @seealso setSpatialDataDir

convertHMSSmoke <- function(
  # dsnPath = NULL, #RC NOTE: removing
  datestamp = NULL,
  nameOnly = FALSE
) {

  # RC NOTE: this vertion of convertHMSSmoke() operates differently, instead of
  # RC NOTE: using pre-downloaded shp files, the datestamp parameter is used to
  # RC NOTE: create the url to download and then convert the data. In the prior
  # RC NOTE: version, datestamp = NULL was supposed to convert all downloaded smoke
  # RC NOTE: files from dsnPath. It actually didn't do this. It did return all filenames
  # RC NOTE: but only converted one.
  # RC NOTE:
  # RC NOTE: In the future we could allow datestamp to be a vector and enhance the
  # RC NOTE: function to loop through all days specified. This could either create multiple
  # RC NOTE: SPDFs or merge them all together. We could also bring back dsnPath parameter
  # RC NOTE: and download if its NULL and use previously downloaded files if specified

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(datestamp)

  # ----- Setup ----------------------------------------------------------------

  # Use package internal data directory
  dataDir <- getSpatialDataDir()

  # Specify the name of the dataset and file being created
  datasetName <- paste0('HMSSmoke_', datestamp)

  if (nameOnly)
    return(datasetName)

  # RC NOTE: commenting out for now bc making a version that downloads based on datestamp

  # if ( is.null(datestamp) ) {
  #
  #   # Create a list of datestamps from files in the directory and call this function
  #   # recursively with Recall(...).
  #   shapefiles <- list.files(dsnPath,pattern='hms_smoke.*\\.shp')
  #   datestamps <- stringr::str_sub(shapefiles,10,17)
  #
  #   #TODO: better acknowledgement of what datestamp is
  #   for ( datestamp in datestamps ) {
  #     result <- try( Recall(dsnPath, datestamp), # 'Recall' is a placedholder for the name of the function in which it is called.
  #                    silent=TRUE)
  #     if ( class(result)[1] == "try-error" ) {
  #       warning(geterrmessage(),'\n')
  #     }
  #   }
  #   datasetNames <- paste0('HMSSmoke_',datestamps)
  #   return(invisible(datasetNames))
  #
  # } else {
  #
  #   # Specify the name of the dataset and file being created
  #   datasetName <- paste0('HMSSmoke_', datestamp)
  #
  #   if (nameOnly)
  #     return(datasetName)
  #
  # }

  # ----- Get the data ---------------------------------------------------------

  # Build appropriate request URL
  url <- paste0(
    "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/Shapefile/2018/01/hms_smoke",
    datestamp,
    ".zip"
  )

  filePath <- file.path(dataDir, basename(url))
  utils::download.file(url, filePath)
  # NOTE:  This zip file has no directory so extra subdirectory needs to be created
  utils::unzip(filePath, exdir = file.path(dataDir))

  # ----- Convert to SPDF ------------------------------------------------------

  # Convert shapefile into SpatialPolygonsDataFrame

  dsnPath <- file.path(dataDir, 'data/oper/newhms/output')
  shpName <- paste0('hms_smoke', datestamp)
  SPDF <- convertLayer(
    dsn = dsnPath,
    layerName = shpName,
    encoding = 'UTF-8'
  )

  # ----- Select useful columns and rename -------------------------------------

  #   > dplyr::glimpse(SPDF@data)
  #   Rows: 7
  #   Columns: 4
  #   $ Satellite <chr> "GOES-EAST", "GOES-EAST", "GOES-EAST", "GOES-EAST", "G ...
  #   $ Start     <chr> "2018001 2002", "2018001 2002", "2018001 2002", "20180 ...
  #   $ End       <chr> "2018001 2312", "2018001 2312", "2018001 2312", "20180 ...
  #   $ Density   <dbl> 5, 5, 5, 5, 5, 5, 5

  # Data Dictionary:
  #   $ Satellite -----> (drop)
  #   $ Start   -----> startTime
  #   $ End      -----> endTime
  #   $ Density  -----> density

  # Calculate latitude and longitude
  # use centroids
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

  # Add countryCode
  SPDF@data$countryCode <- getCountryCode(lon, lat, useBuffering = TRUE)

  # Add stateCode using latitude and longitude
  if ( !exists('NaturalEarthAdm1') ) {
    loadSpatialData('NaturalEarthAdm1')
  }
  SPDF$stateCode <- getStateCode(lon, lat, countryCodes=unique(SPDF$countryCode), useBuffering = TRUE)

  # Add timezone
  SPDF$timezone <- getTimezone(lon, lat, useBuffering =TRUE)

  # Add POSIXct times
  SPDF$startTime <- MazamaCoreUtils::parseDatetime(
    paste0(datestamp, stringr::str_split_fixed(SPDF$Start, " ", 2)[, 2] ),
    timezone = "UTC"
  )
  SPDF$endTime <- MazamaCoreUtils::parseDatetime(
    paste0(datestamp, stringr::str_split_fixed(SPDF$End, " ", 2)[, 2] ),
    timezone = "UTC"
  )

  # Add numeric density
  # NOTE:  data files prior to August 13, 2007 are missing the 'Density' column
  if ('Density' %in% names(SPDF)) {
    SPDF$density <- as.numeric(SPDF@data$Density)
  } else {
    SPDF$density <- as.numeric(NA)
  }

  # Retain useful columns
  SPDF <- SPDF[,c('starttime','endtime','density','longitude',
                  'latitude','countryCode','stateCode','timezone')]

  # Create the new dataframe in a specific column order
  SPDF@data <-
    dplyr::select(
      .data = SPDF@data,
      countryCode = .data$countryCode,
      stateCode = .data$stateCode,
      density = .data$density,
      longitude = lon,
      latitude = lat,
      startTime = .data$startTime,
      endTime = .data$endTime,
      timezone = .data$timezone,
    )

  # ----- Clean SPDF -----------------------------------------------------------

  # RC NOTE: should we add a rownum column so we can do this section with a unique identifier?
  # # Group polygons with the same identifier (stateFIPS)
  # SPDF <- organizePolygons(
  #   SPDF,
  #   uniqueID = 'stateFIPS',
  #   sumColumns = c('landArea', 'waterArea')
  # )
  #
  # # Clean topology errors
  # if ( !cleangeo::clgeo_IsValid(SPDF) ) {
  #   SPDF <- cleangeo::clgeo_Clean(SPDF)
  # }

  # ----- Name and save the data -----------------------------------------------

  # Assign a name and save the data
  message("Saving full resolution version...\n")
  assign(datasetName, SPDF)
  save(list = c(datasetName), file = paste0(dataDir, '/', datasetName, '.rda'))
  rm(list = datasetName)

  # RC NOTE:  skipping simplify section bc this is such a small file

  # ----- Clean up and return --------------------------------------------------

  # Clean up
  unlink(filePath, force = TRUE)
  unlink(dsnPath, recursive = TRUE, force = TRUE)

  return(invisible(datasetName))

}

