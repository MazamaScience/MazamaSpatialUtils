#' @keywords datagen
#' @export

#' @title Convert US congressional districts shapefile

#' @param nameOnly Logical specifying whether to only return the name without 
#' creating the file.
#' @param simplify Logical specifying whether to create "_05", _02" and "_01" 
#' versions of the file that are simplified to 5\%, 2\% and 1\%.

#' @description Returns a SpatialPolygonsDataFrame for US Congressional Districts 
#' for the 116th US House of Representatives.

#' @details A US congressional district shapefile is downloaded and converted to
#' a SpatialPolygonsDataFrame with additional columns of data. The resulting 
#' file will be created in the spatial data directory which is set with 

#' \code{setSpatialDataDir()}.

#' @return Name of the dataset being created.

#' @references \url{https://www.census.gov/geo/maps-data/data/cbf/cbf_cds.html}

#' @seealso setSpatialDataDir

convertUSCensusCongress <- function(
  nameOnly = FALSE,
  simplify = TRUE
) {
  
  # ----- Setup ----------------------------------------------------------------
  
  # Use package internal data directory
  dataDir <- getSpatialDataDir()
  
  # Specify the name of the dataset and file being created
  datasetName <- 'USCensusCongress'
  
  if (nameOnly) 
    return(datasetName)
  
  # ----- Get the data ---------------------------------------------------------

  # Build appropriate request URL 
  # NOTE: 500k means resolution level 1:500k. 
  # RC Note: cd116 means Congressional District (116th Congress)  -- old url: 'http://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_cd115_500k.zip'
  url <- 'https://www2.census.gov/geo/tiger/GENZ2019/shp/cb_2019_us_cd116_500k.zip'
  
  filePath <- file.path(dataDir,basename(url))
  utils::download.file(url,filePath)
  # NOTE:  This zip file has no directory so extra subdirectory needs to be created
  utils::unzip(filePath, exdir = file.path(dataDir, 'congress'))
  
  # ----- Convert to SPDF ------------------------------------------------------

  # Convert shapefile into SpatialPolygonsDataFrame
  # NOTE:  The 'congress' directory has been created
  dsnPath <- file.path(dataDir,'congress')
  shpName <- 'cb_2019_us_cd116_500k'
  SPDF <- convertLayer(
    dsn=dsnPath,
    layerName=shpName,
    encoding = 'UTF-8'
  )
  
  # ----- Select useful columns and rename -------------------------------------
  
  # > dplyr::glimpse(SPDF@data)
  # Rows: 441
  # Columns: 8
  # $ STATEFP  <chr> "48", "26", "35", "17", "17", "17", "05", "06", "06", "06", …
  # $ CD116FP  <chr> "24", "05", "01", "10", "11", "15", "01", "49", "05", "08", …
  # $ AFFGEOID <chr> "5001600US4824", "5001600US2605", "5001600US3501", "5001600 …
  # $ GEOID    <chr> "4824", "2605", "3501", "1710", "1711", "1715", "0501", "06 …
  # $ LSAD     <chr> "C2", "C2", "C2", "C2", "C2", "C2", "C2", "C2", "C2", "C2", …
  # $ CDSESSN  <chr> "116", "116", "116", "116", "116", "116", "116", "116", "11 …
  # $ ALAND    <chr> "680637123", "6083502244", "11916427486", "777275115", "725 …
  # $ AWATER   <chr> "23057547", "4835444214", "16512286", "31723330", "16209000"…
    
  # Given state FIPS code, find state code
  extractState <- function(row) {
    fips <- row['STATEFP']
    stateCode <- MazamaSpatialUtils::US_stateCodes$stateCode[MazamaSpatialUtils::US_stateCodes$stateFIPS==fips]
    return(stateCode)
  }
  
  SPDF@data$stateCode <- as.character(apply(SPDF@data, 1, extractState))
  
  # Remove outlying territories
  SPDF <- subset(SPDF, SPDF@data$stateCode %in% US_52)
  
  # Data Dictionary:
  #   STATEFP -----> stateFIPS: 2-digit FIPS code 
  #   CD116FP -----> congressionalDistrictFIPS
  #   AFFGEOID ----> AFFGEOID    
  #   GEOID -------> GeoID    
  #   LSAD --------> (drop)  
  #   CDSESSN ------> (drop) this is the congressional district session number of the dataset (116 for all) 
  #   ALAND -------> areaLand: land area (in sq. meters)    
  #   AWATER ------> areaWater: water area (in sq. meters) 
  
  # Guarantee that ALAND and AWATER are numeric
  SPDF@data$ALAND <- as.numeric(SPDF@data$ALAND)
  SPDF@data$AWATER <- as.numeric(SPDF@data$AWATER)
  
  SPDF@data$countryCode <- "US"
  
  # Create the new dataframe in a specific column order
  SPDF@data <- 
    dplyr::select(
      .data = SPDF@data,
      countryCode = .data$countryCode,
      stateCode = .data$stateCode,
      stateFIPS = .data$STATEFP,
      congressionalDistrictFIPS = .data$CD116FP,
      areaLand = .data$ALAND,
      areaWater = .data$AWATER,
      AFFGEOID = .data$AFFGEOID,
      GeoID = .data$GEOID
    )
  
  # ----- Clean SPDF -----------------------------------------------------------
 
  # Group polygons with the same identifier (congressionalDistrictFIPS)
  SPDF <- organizePolygons(
    SPDF, 
    uniqueID='GeoID', #RC should I drop GEOID and use AFFGEOID here instead?
    sumColumns=c('areaLand', 'areaWater')
  )
  
  # Clean topology errors
  if ( !cleangeo::clgeo_IsValid(SPDF) ) {
    SPDF <- cleangeo::clgeo_Clean(SPDF)
  }
  
  # ----- Name and save the data -----------------------------------------------

  # Assign a name and save the data
  message("Saving full resolution version...\n")
  assign(datasetName, SPDF)
  save(list = c(datasetName), file = paste0(dataDir,'/', datasetName, '.rda'))
  rm(list = datasetName)
  
  # ----- Simplify -------------------------------------------------------------
  
  if ( simplify ) {
    # Create new, simplified datsets: one with 5%, 2%, and one with 1% of the vertices of the original
    # NOTE:  This may take several minutes.
    message("Simplifying to 5%...\n")
    SPDF_05 <- rmapshaper::ms_simplify(SPDF, 0.05)
    SPDF_05@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
    # Clean topology errors
    if ( !cleangeo::clgeo_IsValid(SPDF_05) ) {
      SPDF_05 <- cleangeo::clgeo_Clean(SPDF_05)
    }
    datasetName_05 <- paste0(datasetName, "_05")
    message("Saving 5% version...\n")
    assign(datasetName_05, SPDF_05)
    save(list = datasetName_05, file = paste0(dataDir,"/", datasetName_05, '.rda'))
    rm(list = c("SPDF_05",datasetName_05))
    
    message("Simplifying to 2%...\n")
    SPDF_02 <- rmapshaper::ms_simplify(SPDF, 0.02)
    SPDF_02@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
    # Clean topology errors
    if ( !cleangeo::clgeo_IsValid(SPDF_02) ) {
      SPDF_02 <- cleangeo::clgeo_Clean(SPDF_02)
    }
    datasetName_02 <- paste0(datasetName, "_02")
    message("Saving 2% version...\n")
    assign(datasetName_02, SPDF_02)
    save(list = datasetName_02, file = paste0(dataDir,"/", datasetName_02, '.rda'))
    rm(list = c("SPDF_02",datasetName_02))
    
    message("Simplifying to 1%...\n")
    SPDF_01 <- rmapshaper::ms_simplify(SPDF, 0.01)
    SPDF_01@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
    # Clean topology errors
    if ( !cleangeo::clgeo_IsValid(SPDF_01) ) {
      SPDF_01 <- cleangeo::clgeo_Clean(SPDF_01)
    }
    datasetName_01 <- paste0(datasetName, "_01")
    message("Saving 1% version...\n")
    assign(datasetName_01, SPDF_01)
    save(list = datasetName_01, file = paste0(dataDir,"/", datasetName_01, '.rda'))
    rm(list = c("SPDF_01",datasetName_01))
  }
  
  # ----- Clean up and return --------------------------------------------------
  
  # Clean up
  unlink(filePath, force = TRUE)
  unlink(dsnPath, recursive = TRUE, force = TRUE)
  
  return(invisible(datasetName))
  
}

  
  
  
  
  
  
  
  
  
  

