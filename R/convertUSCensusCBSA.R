#' @keywords datagen
#' @export
#' @title Convert US Core Based Statistical Areas Shapefile
#' @param nameOnly logical specifying whether to only return the name without creating the file
#' @param simplify logical specifying whether to create "_05", _02" and "_01" versions of the file that are simplified to 5\%, 2\% and 1\%
#' @description Returns a SpatialPolygonsDataFrame for US CBSAs
#' @details A US Core Based Statistical Areas (CBSA) shapefile is downloaded and converted to a 
#' SpatialPolygonsDataFrame with additional columns of data. The resulting file will be created
#' in the spatial data directory which is set with \code{setSpatialDataDir()}.
#' 
#' From the Census Bureau:
#' 
#' Metropolitan and Micropolitan Statistical Areas are together termed Core Based Statistical 
#' Areas (CBSAs) and are defined by the Office of Management and Budget (OMB) and consist of 
#' the county or counties or equivalent entities associated with at least one urban core 
#' (urbanized area or urban cluster) of at least 10,000 population, plus adjacent counties 
#' having a high degree of social and economic integration with the core as measured through 
#' commuting ties with the counties containing the core. Categories of CBSAs are: Metropolitan 
#' Statistical Areas, based on urbanized areas of 50,000 or more population; and Micropolitan 
#' Statistical Areas, based on urban clusters of at least 10,000 population but less than 
#' 50,000 population.
#' 
#' Boundaries are those defined by OMB based on the 2010 Census, published in 2013, and updated in 2015.
#' 
#' @return Name of the dataset being created.
#' @references \url{https://catalog.data.gov/dataset/tiger-line-shapefile-2017-nation-u-s-current-metropolitan-statistical-area-micropolitan-statist}
#' @seealso setSpatialDataDir
#' @seealso getUSCounty
convertUSCensusCBSA <- function(nameOnly=FALSE, simplify=FALSE) {
  
  # Use package internal data directory
  dataDir <- getSpatialDataDir()
  
  # Specify the name of the dataset and file being created
  datasetName <- 'USCensusCBSA'
    
  if (nameOnly) return(datasetName)

  # Build appropriate request URL for US County Borders data
  url <- 'http://www2.census.gov/geo/tiger/TIGER2017/CBSA/tl_2017_us_cbsa.zip'
  
  filePath <- paste(dataDir,basename(url),sep='/')
  utils::download.file(url,filePath)
  # NOTE:  This zip file has no directory so extra subdirectory needs to be created
  utils::unzip(filePath,exdir=paste0(dataDir, '/tl_2017_us_cbsa'))
  
  # Convert shapefile into SpatialPolygonsDataFrame
  # NOTE:  The 'counties' directory has been created
  dsnPath <- paste(dataDir,'tl_2017_us_cbsa',sep='/')
  shpName <- 'tl_2017_us_cbsa'
  SPDF <- convertLayer(dsn=dsnPath, layerName=shpName)###, encoding='latin1')
  
  # Rationalize naming:
  # * human readable full nouns with descriptive prefixes
  # * generally lowerCamelCase
  # with internal standards:
  # * countryCode (ISO 3166-1 alpha-2)
  # * stateCode (ISO 3166-2 alpha-2)
  # * longitude (decimal degrees E)
  # * latitude (decimal degrees N)
  # * area (m^2)
  
  # > head(SPDF@data, 5)
  # CSAFP CBSAFP GEOID             NAME                    NAMELSAD LSAD MEMI MTFCC       ALAND     AWATER    INTPTLAT     INTPTLON
  # 0   462  40340 40340    Rochester, MN    Rochester, MN Metro Area   M1    1 G3110  6415412346   75315930 +43.9499166 -092.3356986
  # 1   450  39580 39580      Raleigh, NC      Raleigh, NC Metro Area   M1    1 G3110  5485063049   76967367 +35.7567464 -078.4604412
  # 2   452  39660 39660   Rapid City, SD   Rapid City, SD Metro Area   M1    1 G3110 20213988388   56100766 +44.1951082 -102.9166120
  # 3   464  40380 40380    Rochester, NY    Rochester, NY Metro Area   M1    1 G3110  8459578202 5126863798 +43.1480380 -077.5232575
  # 4   154  39700 39700 Raymondville, TX Raymondville, TX Micro Area   M2    2 G3110  1529611927  501715022 +26.4803815 -097.5832561
  
  usefulColumns <- c("CBSAFP",  "NAME", "MEMI", "ALAND", "AWATER","INTPTLAT", "INTPTLON")
  SPDF@data <- SPDF@data[usefulColumns]
  names(SPDF@data) <- c("CBSAFP", "CBSAName", "sizeClass", "areaLand", "areaWater","latitude", "longitude")

  # Area already in m^2 according to tl_2017_us_cbsa.shp.xml
  
  # Convert lat/lon to numeric
  SPDF$latitude <- as.numeric(SPDF$latitude)
  SPDF$longitude <- as.numeric(SPDF$longitude)

  # We can use longitude and latitude to get one state code for each polygon.
  SPDF$stateCode <- getStateCode(SPDF$longitude, SPDF$latitude, dataset='USCensusStates', useBuffering=TRUE)
  SPDF$countryCode <- "US"
  
  # Get CBSAName and allStateCodes from the CBSAName column
  nameMatrix <- stringr::str_split_fixed(SPDF$CBSAName, ',', 2)
  SPDF$CBSAName <- nameMatrix[,1]
  # allStateCodes is a comma-separate list of stateCodes
  SPDF$allStateCodes <- stringr::str_trim( stringr::str_replace_all(nameMatrix[,2], '-',',') )
  
  # Convert sizeClass
  metroMask <- SPDF$sizeClass == "1"
  SPDF$sizeClass[metroMask] <- "metro"
  SPDF$sizeClass[!metroMask] <- "micro"
  
  # Group polygons with the same identifier (countyName)
  SPDF <- organizePolygons(SPDF, uniqueID='CBSAFP', sumColumns=c('areaLand','areaWater'))

  # Assign a name and save the data
  cat("Saving full resolution version...\n")
  assign(datasetName,SPDF)
  save(list=c(datasetName),file=paste0(dataDir,'/',datasetName,'.RData'))
  rm(list=datasetName)
  
  if ( simplify ) {
    # Create new, simplified datsets: one with 5%, 2%, and one with 1% of the vertices of the original
    # NOTE:  This may take several minutes. 
    cat("Simplifying to 5%...\n")
    SPDF_05 <- rmapshaper::ms_simplify(SPDF, 0.05)
    SPDF_05@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
    datasetName_05 <- paste0(datasetName, "_05")
    cat("Saving 5% version...\n")
    assign(datasetName_05, SPDF_05)
    save(list=datasetName_05, file = paste0(dataDir,"/",datasetName_05, '.RData'))
    rm(list=c("SPDF_05",datasetName_05))
    
    cat("Simplifying to 2%...\n")
    SPDF_02 <- rmapshaper::ms_simplify(SPDF, 0.02)
    SPDF_02@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
    datasetName_02 <- paste0(datasetName, "_02")
    cat("Saving 2% version...\n")
    assign(datasetName_02, SPDF_02)
    save(list=datasetName_02, file = paste0(dataDir,"/",datasetName_02, '.RData'))
    rm(list=c("SPDF_02",datasetName_02))
    
    cat("Simplifying to 1%...\n")
    SPDF_01 <- rmapshaper::ms_simplify(SPDF, 0.01) 
    SPDF_01@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
    datasetName_01 <- paste0(datasetName, "_01")
    cat("Saving 1% version...\n")
    assign(datasetName_01, SPDF_01)
    save(list=datasetName_01, file = paste0(dataDir,"/",datasetName_01, '.RData'))
    rm(list=c("SPDF_01",datasetName_01))
  }
  # Clean up
  unlink(filePath, force=TRUE)
  unlink(dsnPath, recursive=TRUE, force=TRUE)
  
  return(invisible(datasetName))
}

