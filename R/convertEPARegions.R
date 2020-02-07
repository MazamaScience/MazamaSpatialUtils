#' @keywords datagen
#' @export
#' 
#' @title Convert EPA Region shapefiles
#' 
#' @param nameOnly Logical specifying whether to only return the name without 
#' creating the file.
#' @param simplify Logical specifying whether to create "_05", _02" and "_01" 
#' versions of the file that are simplified to 5\%, 2\% and 1\%.
#' 
#' @return Name of the dataset being created.
#' 
#' @description Returns a SpatialPolygonsDataFrame for EPA Regions
#' 
#' The EPARegions layer is a polygon shapefile coverage representing the 
#' boundaries of the ten Regional Offices of the United States Environmental 
#' Protection Agency in the United States. Each regional office 
#' monitors the environmental regulations within a group of states.
#' 
#' @note Updated 7/26/2019
#' 
#' @references \url{https://www.arcgis.com/home/item.html?id=c670540796584c72b4f59b676ccabe6a}

convertEPARegions <- function(
  nameOnly = FALSE,
  simplify = TRUE
) {
  
  # ----- Setup ---------------------------------------------------------------
  loadSpatialData("USCensusStates")
  
  # Use package internal data directory
  dataDir <- getSpatialDataDir()
  
  # Specify the name of the dataset and file being created
  datasetName <- "EPARegions"
  
  if (nameOnly)
    return(datasetName)
  
  # ----- Get the data ---------------------------------------------------------
  
  # Build appropriate request URL
  url <- 'https://opendata.arcgis.com/datasets/c670540796584c72b4f59b676ccabe6a_3.zip'
  
  filePath <- file.path(dataDir,basename(url))
  utils::download.file(url,filePath)
  # NOTE:  This zip file has no directory so extra subdirectory needs to be created
  utils::unzip(filePath,exdir=file.path(dataDir, 'epa_regions'))
  
  # ----- Convert to SPDF ------------------------------------------------------
  
  # Convert shapefile into SpatialPolygonsDataFrame
  dsnPath <- file.path(dataDir,'epa_regions')
  shpName <- 'Environmental_Protection_Agency_EPA_Regions'
  SPDF <- convertLayer(dsn = dsnPath, layerName = shpName)
  
  # Fix potentially bad topology first.
  # https://postgis.net/workshops/postgis-intro/validity.html#st-buffer
  SPDF <- suppressWarnings( rgeos::gBuffer(SPDF, byid = TRUE, width = 0) )
  
  # ----- Select useful columns and rename -------------------------------------
  
  # dplyr::glimpse(SPDF@data)
  #   Observations: 10
  #   Variables: 4
  #   $ OBJECTID   <chr> "1", "2", "3", "4", "5", "6", ...
  #   $ EPAREGION  <chr> "Region 1", "Region 10", "Regi...
  #   $ Shape_Leng <dbl> 33.10624, 298.86497, 40.51849,...
  #   $ Shape_Area <dbl> 20.97047, 389.03109, 19.30733,...
  
  # Remapped fields:
  #   $ OBJECTID ---> polygonID 
  #   $ EPAREGION --> name
  #   $ Shape_Leng -> (drop)
  #   $ Shape_Area -> (drop)
  
  SPDF@data <- dplyr::select(
    SPDF@data,
    polygonID = .data$OBJECTID,
    name = .data$EPAREGION
  )
  
  # ----- Organize polygons ----------------------------------------------------

  # NOTE: any(duplicated(SPDF@data$polygonID)) == FALSE

  # ----- Add country and state codes ------------------------------------------
  
  SPDF$countryCode <- "US" # TODO?
  
  centroids <- rgeos::gPointOnSurface(SPDF, byid = TRUE)
  SPDF@data$longitude <- centroids$x
  SPDF@data$latitude <- centroids$y

  # Use longitude and latitude to get one state code for each polygon.
  # NOTE: EPA Regions can span multiple states and include overseas territories
  SPDF$stateCode <- getStateCode(
    SPDF@data$longitude, 
    SPDF@data$latitude, 
    dataset = 'USCensusStates', 
    useBuffering = TRUE
  )
  
  # NOTE: Not adding SPDF$allStateCodes because of overseas territory coverage.
  
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

