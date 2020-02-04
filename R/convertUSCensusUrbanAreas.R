#' @keywords datagen
#' @export
#' 
#' @title Convert US Census Urban Areas shapefiles
#' 
#' @param nameOnly Logical specifying whether to only return the name without 
#' creating the file.
#' @param simplify Logical specifying whether to create "_05", _02" and "_01" 
#' versions of the file that are simplified to 5\%, 2\% and 1\%.
#' 
#' @return Name of the dataset being created.
#' 
#' @description Returns a SpatialPolygonsDataFrame for US Census Urban Areas
#' 
#' The USCensusUrbanAreas layer is a polygon shapefile coverage representing the 
#' urban areas delineated by the US Census that represent densely developed 
#' territory.  Encompassing residential, commercial, and other nonresidential 
#' urban land uses, this territory consists of areas of high population density 
#' and urban land use resulting in a representation of the "urban footprint."
#' 
#' There are two types of urban areas: urbanized areas (UAs) that contain 50,000 
#' or more people and urban clusters (UCs) that contain at least 2,500 people, 
#' but fewer than 50,000 people (except in the U.S. Virgin Islands and Guam 
#' which each contain urban clusters with populations greater than 50,000). 
#' Each urban area is identified by a 5-character numeric census code that may 
#' contain leading zeroes.
#' 
#' @details
#' \itemize{
#' \item{\code{urbanAreaID} -- Urban area identifier}
#' \item{\code{name} -- 2010 Census urban area name}
#' \item{\code{urbanAreaType} -- "U" = urbanized area, "C" = urban cluster}
#' \item{\code{landArea} -- land area (in sq. meters)}
#' \item{\code{waterArea} -- water area (in sq. meters)}
#' \item{\code{latitude} -- latitude in decimal degrees}
#' \item{\code{longitude} -- longitude in decimal degrees}
#' }
#' 
#' @note Publication Data: December 2, 2017
#' 
#' @references \url{https://catalog.data.gov/dataset/tiger-line-shapefile-2017-2010-nation-u-s-2010-census-urban-area-national}

convertUSCensusUrbanAreas <- function(
  nameOnly = FALSE,
  simplify = TRUE
) {
  
  # ----- Setup ---------------------------------------------------------------
  loadSpatialData("USCensusStates")
  
  # Use package internal data directory
  dataDir <- getSpatialDataDir()
  
  # Specify the name of the dataset and file being created
  datasetName <- "USCensusUrbanAreas"
  
  if (nameOnly)
    return(datasetName)
  
  # ----- Get the data ---------------------------------------------------------
  
  # Build appropriate request URL
  url <- 'https://www2.census.gov/geo/tiger/TIGER2017/UAC/tl_2017_us_uac10.zip'
  
  filePath <- file.path(dataDir,basename(url))
  utils::download.file(url,filePath)
  # NOTE:  This zip file has no directory so extra subdirectory needs to be created
  utils::unzip(filePath, exdir = file.path(dataDir, 'us_census_urban_areas'))
  
  # ----- Convert to SPDF ------------------------------------------------------
  
  # Convert shapefile into SpatialPolygonsDataFrame
  dsnPath <- file.path(dataDir,'us_census_urban_areas')
  shpName <- 'tl_2017_us_uac10'
  SPDF <- convertLayer(dsn = dsnPath, layerName = shpName)
  
  # ----- Select useful columns and rename -------------------------------------
  
  #   > dplyr::glimpse(SPDF@data)
  #   Observations: 3,601
  #   Variables: 12
  #   $ UACE10     <chr> "24310", "27847", "18100", "06166", "75270...
  #   $ GEOID10    <chr> "24310", "27847", "18100", "06166", "75270...
  #   $ NAME10     <chr> "Dixon, IL", "Escanaba, MI", "Clintonville...
  #   $ NAMELSAD10 <chr> "Dixon, IL Urban Cluster", "Escanaba, MI U...
  #   $ LSAD10     <chr> "76", "76", "76", "76", "76", "75", "76", ...
  #   $ MTFCC10    <chr> "G3500", "G3500", "G3500", "G3500", "G3500...
  #   $ UATYP10    <chr> "C", "C", "C", "C", "C", "U", "C", "C", "U...
  #   $ FUNCSTAT10 <chr> "S", "S", "S", "S", "S", "S", "S", "S", "S...
  #   $ ALAND10    <chr> "25525003", "46648248", "5854683", "304025...
  #   $ AWATER10   <chr> "938058", "283456", "502563", "2314", "0",...
  #   $ INTPTLAT10 <chr> "+41.8529507", "+45.8704839", "+44.6232203...
  #   $ INTPTLON10 <chr> "-089.4817439", "-087.0638396", "-088.7611...
  
  # Data Dictionary:
  #   UACE10 -----> (drop)    
  #   GEOID10 ----> urbanAreaID: Urban area identifier     
  #   NAME10 -----> name: 2010 Census urban area name   
  #   NAMELSAD10 -> (drop)
  #   LSAD10 -----> (drop)
  #   MTFCC10 ----> (drop)   
  #   UATYP10 ----> urbanAreaType: "U" = urbanized area, "C" = urban cluster    
  #   FUNCSTAT10 -> (drop)
  #   ALAND10 ----> landArea: land area (in sq. meters)    
  #   AWATER10 ---> waterArea: water area (in sq. meters)   
  #   INTPTLAT10 -> latitude 
  #   INTPTLON10 -> longitude 
  
  # Convert character fields to numeric and double as needed
  SPDF@data$land <- as.double(SPDF@data$ALAND10)
  SPDF@data$water <- as.double(SPDF@data$AWATER10)
  
  SPDF@data$INTPTLAT10 <- as.numeric(SPDF@data$INTPTLAT10)
  SPDF@data$INTPTLON10 <- as.numeric(SPDF@data$INTPTLON10)
  
  # Convert UATYP10 values into human-readable forms
  is_urban <- SPDF@data$UATYP10 == "U"
  is_cluster <-  SPDF@data$UATYP10 == "C"
  
  SPDF@data$UATYP10[is_urban] <- "urbanizedArea"
  SPDF@data$UATYP10[is_cluster] <- "urbanCluster"
  
  SPDF@data <- dplyr::select(
    SPDF@data,
    urbanAreaID = .data$GEOID10,
    name = .data$NAME10,
    urbanAreaType = .data$UATYP10,
    landArea = .data$ALAND10,
    waterArea = .data$AWATER10,
    longitude = .data$INTPTLON10,
    latitude = .data$INTPTLAT10
  )
  
  # Create unique ID that is just a sequential value
  IDs <- seq(1, length(SPDF))
  SPDF@data$ID <- IDs
  
  # ----- Organize polygons ----------------------------------------------------
  # NOTE: any(duplicated(SPDF@data[["urbanAreaID"]])) == FALSE
  SPDF <- organizePolygons(SPDF, "ID")
  
  # Drop the extraneous ID column
  SPDF@data$ID <- NULL
  
  # ----- Add country and state codes ------------------------------------------
  # Use longitude and latitude to get one state code for each polygon
  SPDF$stateCode <- getStateCode(
    SPDF@data$longitude, 
    SPDF@data$latitude, 
    dataset = 'USCensusStates', 
    useBuffering = TRUE
  )
  
  SPDF$countryCode <- "US"
  
  # ----- Name and save the data -----------------------------------------------
  
  message("Saving full resolution version...\n")
  assign(datasetName, SPDF)
  save(list = c(datasetName), file = paste0(dataDir,'/',datasetName,'.RData'))
  rm(list = datasetName)
  
  # ----- Simplify -------------------------------------------------------------
  
  if ( simplify ) {
    # Create new, simplified datsets: one with 5%, 2%, and one with 1% of the vertices of the original
    # NOTE:  This may take several minutes.
    message("Simplifying to 5%...\n")
    SPDF_05 <- rmapshaper::ms_simplify(SPDF, 0.05)
    SPDF_05@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
    datasetName_05 <- paste0(datasetName, "_05")
    message("Saving 5% version...\n")
    assign(datasetName_05, SPDF_05)
    save(list = datasetName_05, file = paste0(dataDir,"/",datasetName_05, '.RData'))
    rm(list = c("SPDF_05",datasetName_05))
    
    message("Simplifying to 2%...\n")
    SPDF_02 <- rmapshaper::ms_simplify(SPDF, 0.02)
    SPDF_02@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
    datasetName_02 <- paste0(datasetName, "_02")
    message("Saving 2% version...\n")
    assign(datasetName_02, SPDF_02)
    save(list = datasetName_02, file = paste0(dataDir,"/",datasetName_02, '.RData'))
    rm(list = c("SPDF_02",datasetName_02))
    
    message("Simplifying to 1%...\n")
    SPDF_01 <- rmapshaper::ms_simplify(SPDF, 0.01)
    SPDF_01@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
    datasetName_01 <- paste0(datasetName, "_01")
    message("Saving 1% version...\n")
    assign(datasetName_01, SPDF_01)
    save(list = datasetName_01, file = paste0(dataDir,"/",datasetName_01, '.RData'))
    rm(list = c("SPDF_01",datasetName_01))
  }
  
  # ----- Clean up and return --------------------------------------------------
  
  unlink(filePath, force = TRUE)
  unlink(dsnPath, recursive = TRUE, force = TRUE)
  
  return(invisible(datasetName))
  
}

