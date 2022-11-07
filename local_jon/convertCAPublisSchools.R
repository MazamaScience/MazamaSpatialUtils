#' @keywords datagen
#' 
#' # NOTE:  THESE ARE SPATIAL POINTS
#' 
#' @export
#' @importFrom tidyr separate
#' 
#' @title Convert HIFLD Federal Lands shapefiles
#' 
#' @param nameOnly Logical specifying whether to only return the name without
#' creating the file.
#' 
#' @param simplify Logical specifying whether to create "_05", _02" and "_01"
#' versions of the file that are simplified to 5\%, 2\% and 1\%.
#' 
#' @return Name of the datasetName being created.
#' 
#' @description Creates a simple features data frame of U.S. Federal Lands. This
#' data set consists of federally owned or administered lands and Indian
#' Reservations of the United States, Puerto Rico, and the U.S. Virgin Islands.
#' Only areas of 640 acres or more are included.
#' 
#' Source data are obtained from Homeland Infrastructure Foundation-Level Data (HIFLD):
#' \url{https://hifld-geoplatform.opendata.arcgis.com}
#' 
#' @details The datasetName can be downloaded from
#' \url{https://opendata.arcgis.com/datasetNames/2bb32a6e72414e28aaf72f9dc99d3412_0.zip}
#' 
#' @note This is a 7/21/2017 revision of the January 2005 map layer.
#' 
#' @references \url{https://apps.gis.ucla.edu/geodata/datasetName/california-public-schools/resource/69d3ee4f-e126-401e-84df-f88426c60b8e}


convertCAPublicSchools <- function(
  nameOnly = FALSE,
  simplify = TRUE
) {
  
  # ----- Setup ----------------------------------------------------------------
  
  loadSpatialData("USCensusStates")
  
  # Use package internal data directory
  dataDir <- getSpatialDataDir()
  
  # Specify the name of the dataset and file being created
  datasetName <- "CAPublicSchools"
  
  if (nameOnly)
    return(datasetName)
  
  # ----- Get the data ---------------------------------------------------------
  
  # NOTE:  data can be downloaded from URL as a .zip file.
  
  # Build appropriate request URL
  url <- "http://gis.ucla.edu/geodata/datasetName/b918c8a9-6f32-49dc-9671-6ec4f3571a79/resource/69d3ee4f-e126-401e-84df-f88426c60b8e/download/pubschls.zip"
  
  filePath <- file.path(dataDir, basename(url))
  utils::download.file(url,filePath)
  
  utils::unzip(filePath, exdir = dataDir)
  
  # ----- Convert to SFDF ------------------------------------------------------
  
  # Convert shapefile into simple features data frame
  dsnPath <- file.path(dataDir, 'pubschls')
  shpName <- 'pubschls'
  SFDF <- convertLayer(dsn = dsnPath, layer = shpName)
  
  # Original Fields [from `names(SFDF)`] mapped to new names
  # "FID" --------> (drop)        
  # "AREA" -------> (drop)       
  # "PERIMETER" --> (drop)  
  # "FEDLANP020" -> "polygonID": unique id from original "fedlanp020" data
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
  
  # Only keep features where "FEATURE1" != Null (these are not govt. lands)
  SFDF <- subset(SFDF, SFDF$FEATURE1 != "Null")
  
  # ----- Select useful columns and rename -------------------------------------
  
  # Replace "-" with "," in STATE field
  SFDF$STATE <- stringr::str_replace(SFDF$STATE, '-', ',') 
  
  # Fix all FEATURE1 entries that only contain "TVA"
  SFDF$FEATURE1[SFDF$FEATURE1 == "TVA"] <- "Tennessee Valley Authority TVA"  
  
  # Fix FEATURE1 entries that only contain "Metropolitan Washington Airports Authority"
  SFDF$FEATURE1[SFDF$FEATURE1 == "Metropolitan Washington Airports Authority"] <- "Metropolitan Washington Airports Authority MWAA"
  
  # Rename fields that we're keeping
  
  SFDF <- dplyr::select(
    SFDF,
    ID = .data$FEDLANP020,
    primaryLandType = .data$FEATURE1,
    secondaryLandType = .data$FEATURE2,
    tertiaryLandType = .data$FEATURE3,
    agencyCode = .data$AGBUR,
    areaURL = .data$URL,
    primaryName = .data$NAME1,
    secondaryName = .data$NAME2,
    tertiaryName = .data$NAME3,
    allStateCodes = .data$STATE
  )
  
  # ----- Split the xxxxLandType fields into separate LandType & LandOwner -----
  # TO DO: figure out how to make this fit in 80 char
  agency_regexp <- "(?=\\sBIA|\\sBLM|\\sBOR|\\sDOD|\\sFS|\\sFWS|\\sNPS|\\sOTHER|\\sDOE|\\sDOJ|\\sNASA|\\sARS|\\sGSA|\\sDOT|\\sUSDA|\\sCIA|\\sTVA|\\sMWAA)"
  
  SFDF <- 
    SFDF %>% 
    tidyr::separate(col = "primaryLandType",
                    c("primaryLandType", "primaryLandOwner"),
                    sep = agency_regexp)
  
  # Remove the space left in primaryLandOwner field
  SFDF$primaryLandOwner <- stringr::str_replace(SFDF$primaryLandOwner, ' ', '')
  
  #Split secondaryLandType ins secondaryLandOwner -------------------------
  SFDF <- 
    SFDF %>% 
    tidyr::separate(col = "secondaryLandType",
                    c("secondaryLandType", "secondaryLandOwner"),
                    sep = agency_regexp)
  
  # Remove the space left in secondaryLandOwner field
  SFDF$secondaryLandOwner <- stringr::str_replace(SFDF$secondaryLandOwner, ' ', '')
  
  
  # Split secondaryLandType ins tertiaryLandOwner -------------------------
  SFDF <- 
    SFDF %>% 
    tidyr::separate(col = "tertiaryLandType",
                    c("tertiaryLandType", "tertiaryLandOwner"),
                    sep = agency_regexp)
  
  # Remove the space left in tertiaryLandOwner field
  SFDF$tertiaryLandOwner <- stringr::str_replace(SFDF$tertiaryLandOwner, ' ', '')
  
  # NOTE:  All records should have a primaryLandOwner, check
  # NOTE:    View(subset(SFDF, is.na(primaryLandOwner)) == 0)
  
  # ----- Organize polygons ----------------------------------------------------
  # any(duplicated(SFDF$ID)) is FALSE
  SFDF <- organizePolygons(SFDF, "ID")
  
  # Drop the extraneous ID column
  SFDF$ID <- NULL
  
  # ----- Add stateCode --------------------------------------------------------
  
  # Get latitude and longitude from polygon centroids 
  centroids <- rgeos::gCentroid(SFDF, byid = TRUE)
  lon <- sp::coordinates(centroids)[,1]
  lat <- sp::coordinates(centroids)[,2]
  
  SFDF$longitude <- lon
  SFDF$latitude <- lat
  
  # Use longitude and latitude to get one state code for each polygon
  SFDF$stateCode <- getStateCode(
    SFDF$longitude, 
    SFDF$latitude, 
    datasetName = 'USCensusStates', 
    useBuffering = TRUE
  )
  
  # ----- Add country code -----------------------------------------------------
  SFDF$countryCode <- "US"
  
  # ----- Name and save the data -----------------------------------------------
  message("Saving full resolution version...\n")
  assign(datasetName, SFDF)
  save(list = c(datasetName), file = paste0(dataDir,'/',datasetName,'.RData'))
  rm(list = datasetName)
  
  # ----- Simplify -------------------------------------------------------------
  
  if ( simplify ) {
    # Create new, simplified datsets: one with 5%, 2%, and one with 1% of the vertices of the original
    # NOTE:  This may take several minutes.
    message("Simplifying to 5%...\n")
    SFDF_05 <- rmapshaper::ms_simplify(SFDF, 0.05)
    SFDF_05@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
    datasetName_05 <- paste0(datasetName, "_05")
    message("Saving 5% version...\n")
    assign(datasetName_05, SFDF_05)
    save(list = datasetName_05, file = paste0(dataDir,"/",datasetName_05, '.RData'))
    rm(list = c("SFDF_05",datasetName_05))
    
    message("Simplifying to 2%...\n")
    SFDF_02 <- rmapshaper::ms_simplify(SFDF, 0.02)
    SFDF_02@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
    datasetName_02 <- paste0(datasetName, "_02")
    message("Saving 2% version...\n")
    assign(datasetName_02, SFDF_02)
    save(list = datasetName_02, file = paste0(dataDir,"/",datasetName_02, '.RData'))
    rm(list = c("SFDF_02",datasetName_02))
    
    message("Simplifying to 1%...\n")
    SFDF_01 <- rmapshaper::ms_simplify(SFDF, 0.01)
    SFDF_01@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
    datasetName_01 <- paste0(datasetName, "_01")
    message("Saving 1% version...\n")
    assign(datasetName_01, SFDF_01)
    save(list = datasetName_01, file = paste0(dataDir,"/",datasetName_01, '.RData'))
    rm(list = c("SFDF_01",datasetName_01))
  }
  
  # ----- Clean up and return --------------------------------------------------
  
  unlink(filePath, force = TRUE)
  unlink(dsnPath, recursive = TRUE, force = TRUE)
  
  return(invisible(datasetName))
  
}

