#' @keywords datagen
#' @export
#' 
#' @title Convert Geographic Area Coordination Center geojson, as defined by NIFC
#' 
#' @param nameOnly logical specifying whether to only return the name without creating the file
#' @param simplify Logical specifying whether to create "_05", _02" and "_01" 
#' versions of the file that are simplified to 5\%, 2\% and 1\%.
#' 
#' @description Returns a SpatialPolygonsDataFrame for Geographic Area Coordination Centers (GACCs)
#' 
#' @details A GACC shapefile is downloaded and converted to a 
#' SpatialPolygonsDataFrame with additional columns of data. The resulting file will be created
#' in the spatial data directory which is set with \code{setSpatialDataDir()}.
#' @return Name of the dataset being created.
#' 
#' @references \url{https://hub.arcgis.com/datasets/7dc5f4a286bd47e0aaafa0ab05302fe9_0}
#' 
#' @seealso setSpatialDataDir
#' 
convertGACC <- function(nameOnly=FALSE, simplify = TRUE) {
  
  # ----- Setup ----------------------------------------------------------------
  
  # Use package internal data directory
  dataDir <- getSpatialDataDir()
  
  # Specify the name of the dataset and file being created
  datasetName <- 'GACC'
  
  if (nameOnly) return(datasetName)
  
  # ----- Get the data ---------------------------------------------------------
  
  # NOTE: Update shows it's no longer a geojson file
  # We can unzip the .zip and work with the shapefile directly without conversion
  url <- "https://opendata.arcgis.com/datasets/7dc5f4a286bd47e0aaafa0ab05302fe9_0.zip"
  
  filePath <- file.path(dataDir,basename(url))
  utils::download.file(url,filePath)
  # NOTE:  This zip file has no directory so extra subdirectory needs to be created
  utils::unzip(filePath,exdir=file.path(dataDir,'gacc'))
  
  # ----- Convert to SPDF ------------------------------------------------------
  
  # Convert shapefile into SpatialPolygonsDataFrame
  # NOTE: Prior to update, it read in as geojson file
  
  dsnPath <- file.path(dataDir,'gacc')
  shpName <- 'National_GACC_Current_20200226'
  SPDF <- convertLayer(dsn = dsnPath, layerName = shpName)
  
  
  # > names(SPDF@data)
  # [1] "FID"        "GeometryID" "GACCName"   "GACCUnitID" "GACCAbbrev" "GACCLocati" "ContactPho" "Comments"   "DateCurren" "MapMethod"  "PrepLevel" 
  # [12] "PL_GACC_ID" "SHAPE_Leng" "SHAPE_Area"
  # ----- Select useful columns and rename -------------------------------------
  
  # Rationalize naming:
  # * human readable full nouns with descriptive prefixes
  # * generally lowerCamelCase
  # with internal standards:
  # * countryCode (ISO 3166-1 alpha-2)
  # * stateCode (ISO 3166-2 alpha-2)
  # * longitude (decimal degrees E)
  # * latitude (decimal degrees N)
  # * area (m^2)
  
  # Standardize naming in the SpatialPolygonsDataFrame
  # NOTE: Updated from 2017 data to 2020. 
  # What is the equivalence to GACC_NWCG_Code? Perhaps PL_GACC_ID?
  # Changed old column GACCLabel to GACCAbbrev
  
  SPDF@data <- dplyr::select(.data = SPDF@data, 
                             National_GACC_2020 = .data$FID,
                             uniID = .data$GACCUnitID,
                             GACCName = .data$GACCName, 
                             label = .data$GACCAbbrev,
                             location = .data$GACCLocati, 
                             contactPhone = .data$ContactPho)
                             #GACC_NWCG_Code = .data$GACC_NWCG_Code)
  
  SPDF$countryCode <- 'US'
  SPDF$stateCode <- stringr::str_extract(SPDF$location, "[[:upper:]]{2}$")
  
  # > head(SPDF@data)
  # National_GACC_2020    uniID                                                GACCName label      location contactPhone countryCode stateCode
  # 0                  1  USAKACC                  Alaska Interagency Coordination Center  AICC Fairbanks, AK 907-356-5680          US        AK
  # 1                  2 USWIEACC                        Eastern Area Coordination Center  EACC Milwaukee, WI 414-944-3811          US        WI
  # 2                  3  USUTGBC                         Great Basin Coordination Center  GBCC     Boise, ID 800-844-5497          US        ID
  # 3                  4 USCAONCC Northern California Geographic Area Coordination Center  ONCC   Redding, CA 530-226-2801          US        CA
  
  # ----- Organize polygons ----------------------------------------------------
  
  
  # ----- Name and save the data -----------------------------------------------

  # Assign a name and save the data
  message("Saving full resolution version...\n")
  assign(datasetName, SPDF)
  save(list = c(datasetName), file = paste0(dataDir,'/', datasetName, '.rda'))
  rm(list = datasetName)
  
  # ----- Simplify -------------------------------------------------------------
  
  # simplify
  if ( simplify ) {
    # Create new, simplified datsets: one with 5%, 2%, and one with 1% of the vertices of the original
    # NOTE:  This may take several minutes.
    message("Simplifying to 5%...\n")
    SPDF_05 <- rmapshaper::ms_simplify(SPDF, 0.05)
    SPDF_05@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
    datasetName_05 <- paste0(datasetName, "_05")
    message("Saving 5% version...\n")
    assign(datasetName_05, SPDF_05)
    save(list = datasetName_05, file = paste0(dataDir,"/",datasetName_05, '.rda'))
    rm(list = c("SPDF_05",datasetName_05))
    
    message("Simplifying to 2%...\n")
    SPDF_02 <- rmapshaper::ms_simplify(SPDF, 0.02)
    SPDF_02@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
    datasetName_02 <- paste0(datasetName, "_02")
    message("Saving 2% version...\n")
    assign(datasetName_02, SPDF_02)
    save(list = datasetName_02, file = paste0(dataDir,"/",datasetName_02, '.rda'))
    rm(list = c("SPDF_02",datasetName_02))
    
    message("Simplifying to 1%...\n")
    SPDF_01 <- rmapshaper::ms_simplify(SPDF, 0.01)
    SPDF_01@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
    datasetName_01 <- paste0(datasetName, "_01")
    message("Saving 1% version...\n")
    assign(datasetName_01, SPDF_01)
    save(list = datasetName_01, file = paste0(dataDir,"/",datasetName_01, '.rda'))
    rm(list = c("SPDF_01",datasetName_01))
  }
  
  # ----- Clean up and return --------------------------------------------------
  
  return(invisible(datasetName))
}

