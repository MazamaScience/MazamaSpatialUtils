#' @keywords datagen
#' @export
#' 
#' @title Convert TODO shapefiles
#' 
#' @param nameOnly Logical specifying whether to only return the name without 
#' creating the file.
#' @param simplify Logical specifying whether to create "_05", _02" and "_01" 
#' versions of the file that are simplified to 5\%, 2\% and 1\%.
#' 
#' @return Name of the dataset being created.
#' 
#' @description Returns a SpatialPolygonsDataFrame for TODO
#' 
#' The TODO layer is a polygon shapefile coverage representing TODO.
#' 
#' TODO: More details
#' 
#' @note TODO: MONTH, YEAR version.
#' 
#' @references \url{TODO: DATA_URL}

convertTODO <- function(
  nameOnly = FALSE,
  simplify = TRUE
) {
  
  # ----- Setup ---------------------------------------------------------------
  
  # Use package internal data directory
  dataDir <- getSpatialDataDir()
  
  # NOTE:  Dataset names should be lowerCamelCase with now abbreviations
  # NOTE:  except for known acronymes.
  
  # Specify the name of the dataset and file being created
  datasetName <- "HIFLDFederalLands"
  
  if (nameOnly)
    return(datasetName)
  
  # ----- Get the data ---------------------------------------------------------
  
  # NOTE:  Ideally, data can be downloaded from a spcific URL as a .zip file.
  # NOTE:  In this case, the following example code is a good template.
  
  # Build appropriate request URL
  url <- "https://opendata.arcgis.com/datasets/2bb32a6e72414e28aaf72f9dc99d3412_0.zip"
  
  filePath <- file.path(dataDir,basename(url))
  utils::download.file(url,filePath)
  # NOTE:  This zip file has no directory so extra subdirectory needs to be created
  utils::unzip(filePath,exdir=file.path(dataDir, 'hifld_fed_lands'))
  
  # ----- Convert to SPDF ------------------------------------------------------
  
  # NOTE:  You have to look at the downloaded data to determin shpName
  
  # Convert shapefile into SpatialPolygonsDataFrame
  dsnPath <- file.path(dataDir,'hifld_fed_lands')
  shpName <- 'Federal_Lands'
  SPDF <- convertLayer(dsn = dsnPath, layerName = shpName)
  
  # ORIGINAL DATA LOOKS LIKE...
  #    FID        AREA PERIMETER FEDLANP020                  FEATURE1               FEATURE2 FEATURE3 AGBUR  URL
  # 0 3001 0.000243925 0.0629446      38752                      Null                   <NA>     <NA>  <NA> <NA>
  # 1 3002 0.000272340 0.0667056      38851                      Null                   <NA>     <NA>  <NA> <NA>
  # 2 3003 0.000882379 0.1434380      38888                      Null                   <NA>     <NA>  <NA> <NA>
  # 3 3004 0.070895200 1.0945700      38915 Wilderness Study Area BLM Public Domain Land BLM     <NA>   BLM <NA>
  # 4 3005 0.003993660 0.4887370      39189                      Null                   <NA>     <NA>  <NA> <NA>
  #   NAME1 NAME2 NAME3 STATE STATE_FIPS Shape_Leng   SHAPE__Are SHAPE__Len
  # 0                                   <NA>  <NA>  <NA>  <NA>       <NA> 0.06294457 0.0002439255 0.06294457
  # 1                                   <NA>  <NA>  <NA>  <NA>       <NA> 0.06670559 0.0002723400 0.06670559
  # 2                                   <NA>  <NA>  <NA>  <NA>       <NA> 0.14343765 0.0008823793 0.14343765
  # 3 Mormon Mountains Wilderness Study Area  <NA>  <NA>  NV         32 1.09456795 0.0708952214 1.09456795
  # 4                                   <NA>  <NA>  <NA>  <NA>       <NA> 0.48873716 0.0039936604 0.48873716
  
  # Original Fields [from `names(SPDF@data)`] mapped to new names
  # "FID" --------> (drop)        
  # "AREA" -------> (drop)       
  # "PERIMETER" --> (drop)  
  # "FEDLANP020" -> "areaID": unique id from original "fedlanp020" data
  # "FEATURE1" ---> "primaryLandType": primary land type and owning agency      
  # "FEATURE2" ---> "secondaryLandType": secondary land type and owning agency    
  # "FEATURE3" ---> "tertiaryLandType": tertiary land type and owning agency   
  # "AGBUR" ------> "agencyCode": owning or administering agency code    
  # "URL" --------> "areaURL":  area URL    
  # "NAME1" ------> "primaryName": name associated with primaryLandType      
  # "NAME2" ------> "secondaryName": name associate with secondaryLandType      
  # "NAME3" ------> "tertiaryName": name associated with tertiaryLandType      
  # "STATE" ------> "stateCode": 2-digit state code(s) for where area is located      
  # "STATE_FIPS" -> "stateFIPS": 2-digit, numeric, Federal state identifier code
  # "Shape_Leng" -> (drop)
  # "SHAPE__Are" -> (drop)
  # "SHAPE__Len" -> (drop)
  
  # ----- Select useful columns and rename -------------------------------------
  
  # NOTE:  Convert names to human readable, unabbreviated, lowerCamelCase names.
  # NOTE:  Reasonable names will be constructed as "subTypeNoun" e.g.:
  # NOTE:  waterSurfaceArea, landSurfaceArea, k12StudentCount, collegeStudentCount
  #
  SPDF@data <- dplyr::select(
    SPDF@data,
    areaID = .data$FEDLANP020,
    primaryLandType = .data$FEATURE1,
    secondaryLandType = .data$FEATURE2,
    tertiaryLandType = .data$FEATURE3,
    agencyCode = .data$AGBUR,
    areaURL = .data$URL,
    primaryName = .data$NAME1,
    secondaryName = .data$NAME2,
    tertiaryName = .data$NAME3,
    stateCode = .data$STATE,
    stateFIPS = .data$STATE_FIPS
  )
  
  # ----- Organize polygons ----------------------------------------------------
  
  # any(duplicated(SPDF@data$areaID)) is FALSE
  
  # ----- Add country and state codes ------------------------------------------
  
  # NOTE:  Several functions allow filtering by countryCode and stateCode as a
  # NOTE:  way of reducing the number of polygons that need to be searched.
  # NOTE:  These variables should be included inevery dataset.
  # NOTE:
  # NOTE:  These might be assumed or read from a column (that might be coded in
  # NOTE:  some other way). Or, you might have to use MazamaSpatialUtils::getStateCode()
  # NOTE:  on the polygon centers.
  
  SPDF$countryCode <- "US" # TODO?
  # SPDF$stateCode <- "CA"   # TODO?
  
  # NOTE:  Some datasets may also wish to include SPDF$allStateCodes to show
  # NOTE:  all of the states that overlap with each polygon. An example where
  # NOTE:  this is done is convertWBDHUC.R. (judgement call)
  
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

