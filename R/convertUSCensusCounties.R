#' @keywords datagen
#' @export
#' @title Convert US County Borders Shapefile
#' @param nameOnly logical specifying whether to only return the name without creating the file
#' @description Returns a SpatialPolygonsDataFrame for a US county divisions
#' @details A US county borders shapefile is downloaded and converted to a 
#' SpatialPolygonsDataFrame with additional columns of data. The resulting file will be created
#' in the spatial data directory which is set with \code{setSpatialDataDir()}.
#' @return Name of the dataset being created.
#' @references \url{https://www2.census.gov/geo/tiger/GENZ2019/}
#' @seealso setSpatialDataDir
#' @seealso getUSCounty
convertUSCensusCounties <- function(nameOnly=FALSE) {

  # ----- Setup ----------------------------------------------------------------

  # Use package internal data directory
  dataDir <- getSpatialDataDir()
  
  # Specify the name of the dataset and file being created
  datasetName <- 'USCensusCounties'
    
  if (nameOnly) return(datasetName)
  
  # ----- Get the data ---------------------------------------------------------
  
  # Build appropriate request URL for US County Borders data
  # NOTE: 500k means resolution level 1:500k. 
  url <- 'https://www2.census.gov/geo/tiger/GENZ2019/shp/cb_2019_us_county_500k.zip'
  
  filePath <- file.path(dataDir,basename(url))
  utils::download.file(url,filePath)
  # NOTE:  This zip file has no directory so extra subdirectory needs to be created
  utils::unzip(filePath,exdir=file.path(dataDir,'counties'))
  
  # ----- Convert to SPDF ------------------------------------------------------
  
  # Convert shapefile into SpatialPolygonsDataFrame
  # NOTE:  The 'counties' directory has been created
  dsnPath <- file.path(dataDir, 'counties')
  shpName <- 'cb_2019_us_county_500k'
  SPDF <- convertLayer(dsn = dsnPath, layerName = shpName, encoding = 'latin1')

  # > head(SPDF@data)
  #   STATEFP COUNTYFP COUNTYNS       AFFGEOID GEOID     NAME LSAD      ALAND    AWATER
  # 0      48      081 01383826 0500000US48081 48081     Coke   06 2361153195  42331832
  # 1      48      273 01383922 0500000US48273 48273  Kleberg   06 2282572445 541041659
  # 2      48      203 01383887 0500000US48203 48203 Harrison   06 2331138836  40651525
  
  # Rationalize naming:
  # * human readable full nouns with descriptive prefixes
  # * generally lowerCamelCase
  # with internal standards:
  # * countryCode (ISO 3166-1 alpha-2)
  # * stateCode (ISO 3166-2 alpha-2)
  # * longitude (decimal degrees E)
  # * latitude (decimal degrees N)
  # * area (m^2)
  
  # ----- Select useful columns and rename -------------------------------------
  
  #  Given row of USCensusCounties data, find state code, name, or adm1_code
  extractState <- function(row) {
    fips <- row['stateFIPS']
    stateCode <- MazamaSpatialUtils::US_stateCodes$stateCode[MazamaSpatialUtils::US_stateCodes$fips==paste0("US", fips)]
    return(stateCode)
  }
  
  # Standardize naming in the SpatialPolygonsDataFrame
  
  # TODO:  Figure out units for ALAND and AWATER and convert to m^2
  
  # Guarantee that ALAND and AWATER are numeric
  SPDF$ALAND <- as.numeric(SPDF$ALAND)
  SPDF$AWATER <- as.numeric(SPDF$AWATER)
  
  # New CountyFIPS: Concat STATEFP+COUNTYFP                   
  SPDF$countyFIPS <-with(SPDF@data,paste0(STATEFP,COUNTYFP))
  
  
  SPDF@data <- dplyr::select(.data = SPDF@data, 
                             countyFIPS = .data$countyFIPS,   
                             areaLand = .data$ALAND,
                             areaWater = .data$AWATER,
                             countyName = .data$NAME,
                             stateFIPS = .data$STATEFP,
                             county = .data$COUNTYFP,         
                             COUNTYNS = .data$COUNTYNS) 
  SPDF$stateCode <- apply(SPDF@data, 1, extractState)
  SPDF$countryCode <- "US"
  SPDF$name <- SPDF$countyName
  
  # ----- Organize polygons ----------------------------------------------------
  
  # TODO:  COUNTYNS is the polygon uniqueID but what is it?
  
  # Group polygons with the same identifier (countyName)
  SPDF <- organizePolygons(SPDF, uniqueID='COUNTYNS', sumColumns=c('areaLand','areaWater'))
  
  # ----- Simplify -------------------------------------------------------------
  
  
  # ----- Name and save the data -----------------------------------------------
  
  # Assign a name and save the data
  assign(datasetName,SPDF)
  save(list=c(datasetName),file=paste0(dataDir,'/',datasetName,'.RData'))
  
  
  # ----- Clean up and return --------------------------------------------------
  
  # Clean up
  unlink(filePath, force=TRUE)
  unlink(dsnPath, recursive=TRUE, force=TRUE)
  
  return(invisible(datasetName))
}

