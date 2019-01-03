#' @keywords datagen
#' @export
#' @title Convert California Air Resources Board basin shapefiles
#' @param nameOnly Logical specifying whether to only return the name without 
#' creating the file.
#' @param simplify Logical specifying whether to create "_05", _02" and "_01" 
#' versions of the file that are simplified to 5\%, 2\% and 1\%.
#' @return Name of the dataset being created.
#' @description Returns a SpatialPolygonsDataFrame for CARB air basins,
#' 
#' The California Air Basins layer is a polygon shapefile coverage representing 
#' the 15 California air basins, as defined in state statute and regulation.  
#' See the California Health and Safety Code, Section 39606 et seq. and 
#' California Code of Regulations, Title 17, Section 60100 et seq.
#' 
#' Air Basins are designated pursuant to California statute and regulation.  
#' Air Basins identify regions of similar meteorological and geographic conditions 
#' and consideration for political boundary lines, and are related to air 
#' pollution and its transport.
#' @note March, 2004 version.
#' @references \url{https://www.arb.ca.gov/ei/gislib/gislib.htm}

convertCARBAirBasins <- function(nameOnly=FALSE, simplify=FALSE) {

  # Use package internal data directory
  dataDir <- getSpatialDataDir()
  
  # Specify the name of the dataset and file being created
  datasetName <- 'CA_AirBasins'
  
  if (nameOnly) return(datasetName)
  
  # Build appropriate request URL
  url <- 'https://www.arb.ca.gov/ei/gislib/boundaries/ca_air_basins.zip'
  
  filePath <- file.path(dataDir,basename(url))
  utils::download.file(url,filePath)
  # NOTE:  This zip file has no directory so extra subdirectory needs to be created
  utils::unzip(filePath,exdir=file.path(dataDir, 'ca_air_basins'))
  
  # Convert shapefile into SpatialPolygonsDataFrame
  # NOTE:  The 'counties' directory has been created
  dsnPath <- file.path(dataDir,'ca_air_basins')
  shpName <- 'CaAirBasin'
  SPDF <- convertLayer(dsn=dsnPath, layerName=shpName)
  
  # Rationalize naming:

  # > head(SPDF@data, 5)
  #          AREA PERIMETER CAABA_ CAABA_ID              NAME
  # 0 31839198681 1566653.5      2        2       North Coast
  # 1 39557878115 1279107.6      3        1 Northeast Plateau
  # 2 39178615821 1291238.4      4        3 Sacramento Valley
  # 3 32161425221 1281190.9      5        4 Mountain Counties
  # 4  3446577486  359048.3      6        5       Lake County
  
  usefulColumns <- c("CAABA_", "CAABA_ID", "NAME")
  SPDF@data <- SPDF@data[,usefulColumns]
  names(SPDF@data) <- c("CAABA_", "CAABA_ID", "name")


  # Add core metadata
  SPDF$countryCode <- "US"
  SPDF$stateCode <- "CA"

  # Assign a name and save the data
  message("Saving full resolution version...\n")
  assign(datasetName,SPDF)
  save(list=c(datasetName),file=paste0(dataDir,'/',datasetName,'.RData'))
  rm(list=datasetName)
  
  if ( simplify ) {
    # Create new, simplified datsets: one with 5%, 2%, and one with 1% of the vertices of the original
    # NOTE:  This may take several minutes. 
    message("Simplifying to 5%...\n")
    SPDF_05 <- rmapshaper::ms_simplify(SPDF, 0.05)
    SPDF_05@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
    datasetName_05 <- paste0(datasetName, "_05")
    message("Saving 5% version...\n")
    assign(datasetName_05, SPDF_05)
    save(list=datasetName_05, file = paste0(dataDir,"/",datasetName_05, '.RData'))
    rm(list=c("SPDF_05",datasetName_05))
    
    message("Simplifying to 2%...\n")
    SPDF_02 <- rmapshaper::ms_simplify(SPDF, 0.02)
    SPDF_02@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
    datasetName_02 <- paste0(datasetName, "_02")
    message("Saving 2% version...\n")
    assign(datasetName_02, SPDF_02)
    save(list=datasetName_02, file = paste0(dataDir,"/",datasetName_02, '.RData'))
    rm(list=c("SPDF_02",datasetName_02))
    
    message("Simplifying to 1%...\n")
    SPDF_01 <- rmapshaper::ms_simplify(SPDF, 0.01) 
    SPDF_01@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
    datasetName_01 <- paste0(datasetName, "_01")
    message("Saving 1% version...\n")
    assign(datasetName_01, SPDF_01)
    save(list=datasetName_01, file = paste0(dataDir,"/",datasetName_01, '.RData'))
    rm(list=c("SPDF_01",datasetName_01))
  }
  # Clean up
  unlink(filePath, force=TRUE)
  unlink(dsnPath, recursive=TRUE, force=TRUE)
  
  return(invisible(datasetName))
  
}

