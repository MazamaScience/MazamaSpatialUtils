#' @keywords datagen
#' @export
#' @title Convert MTBS Burn Area shapefiles
#' @param nameOnly Logical specifying whether to only return the name without 
#' creating the file.
#' @param simplify Logical specifying whether to create "_05", _02" and "_01" 
#' versions of the file that are simplified to 5\%, 2\% and 1\%.
#' @return Name of the dataset being created.
#' 
#' @description Returns a SpatialPolygonsDataFrame for all large wildland fires 
#' (includes wildfire, wildland fire use, and prescribed fire) in the 
#' conterminous United States (CONUS), Alaska, Hawaii, and Puerto Rico for the 
#' period of 1984 through 2017.  All fires reported as greater than 1,000 acres 
#' in the western U.S. and greater than 500 acres in the eastern U.S. are 
#' mapped across all ownerships.
#' 
#' @note Data last updated by USFS: August 22, 2019
#' 
#' @references \url{https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/MTBS_Fire/data/composite_data/burned_area_extent_shapefile/mtbs_perimeter_data.zip}

convertMTBSBurnArea <- function(
  nameOnly = FALSE,
  simplify = FALSE
) {
  
  # ----- Setup ---------------------------------------------------------------
  
  # Use package internal data directory
  dataDir <- getSpatialDataDir()
  
  # NOTE:  Dataset names should be lowerCamelCase with now abbreviations
  # NOTE:  except for known acronymes.
  
  # Specify the name of the dataset and file being created
  datasetName <- "MTBSBurnArea"
  
  if (nameOnly)
    return(datasetName)
  
  # ----- Get the data ---------------------------------------------------------
  
  # Build appropriate request URL, download and unzip data
  url <- 'https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/MTBS_Fire/data/composite_data/burned_area_extent_shapefile/mtbs_perimeter_data.zip'
  
  filePath <- file.path(dataDir,basename(url))
  utils::download.file(url,filePath)
  
  utils::unzip(filePath,exdir=dataDir)
  
  # ----- Convert to SPDF ------------------------------------------------------
  
  # Convert shapefile into SpatialPolygonsDataFrame
  dsnPath <- file.path(dataDir,'mtbs_perimeter_data')
  shpName <- 'mtbs_perims_DD'
  SPDF <- convertLayer(dsn = dsnPath, layerName = shpName)
  
  # ----- Select useful columns and rename -------------------------------------
  
  # > head(SPDF@data, 5)
  #                 Fire_ID       Fire_Name    Year StartMonth StartDay  Fire_Type Acres
  #   0 AK5674215793820060522     MESHIK RIVER 2006          5       22  Wildfire  1520
  #   1 AK5759815753720050529      PILOT POINT 2005          5       29  Wildfire  4482
  #   2 AK5761615232120150828      TWIN CREEKS 2015          8       28  Wildfire  3419
  #   3 AK5884115754320150622 COPENHAGEN CREEK 2015          6       22  Wildfire  5335
  #   4 AK5884415640120150621      PAULS CREEK 2015          6       21  Wildfire  9684
  
  # Create stateCode from the 1st 2 letters of the Fire_ID
  SPDF@data$state <- substr(SPDF@data$Fire_ID, 1,2)
  
  SPDF@data <- dplyr::select(
    SPDF@data,
    stateCode = .data$state,
    fireID = .data$Fire_ID,
    fireName = .data$Fire_Name,
    year = .data$Year,
    startMonth = .data$StartMonth,
    startDay = .data$StartDay,
    fireType = .data$Fire_Type,
    acres = .data$Acres
  )
  
  # ----- Organize polygons ----------------------------------------------------
  
  # Create unique ID that is just a sequential value
  IDs <- seq(1, length(SPDF))
  SPDF@data$ID <- IDs
  
  # Get correct lat/lon centroids for new polygons
  # Fix potentially bad topology first.
  # Example error shown below when you DON'T fix these errors
  # Error in TopologyFunc(spgeom, id, byid, "rgeos_pointonsurface") :
  #   TopologyException: Input geom 1 is invalid: Self-intersection at or near
  #   point -119.73617914821601 38.0183025223321 at -119.73617914821601 38.0183025223321
  SPDF <- suppressWarnings( rgeos::gBuffer(SPDF, byid=TRUE, width=0) )
  centroids <- rgeos::gPointOnSurface(SPDF, byid=TRUE)
  
  SPDF@data$longitude <- centroids$x
  SPDF@data$latitude <- centroids$y
  
  # ----- Add country and state codes ------------------------------------------
  SPDF$countryCode <- "US"
  # stateCode already present
  
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

