#' @keywords datagen
#' @export
#' 
#' @title Convert HIFLD Federal Lands shapefiles
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

library(tidyverse)

convertHIFLDFederalLands <- function(
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
  
  # ----- Delete features where "FEATURE1" = Null (these are not govt. lands)
  SPDF <- subset(SPDF, FEATURE1 != "Null")
  
  # ----- Select useful columns and rename -------------------------------------
  
  # Replace "-" with "," in STATE field
  SPDF@data$STATE <- stringr::str_replace(SPDF@data$STATE, '-', ',') 
  
  # NOTE:  including SPDF$allStateCodes to show all of the states that overlap 
  # with each polygon.
  
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
    allStateCodes = .data$STATE,
    stateFIPS = .data$STATE_FIPS
  )
  
  # ----- Organize polygons ----------------------------------------------------
  # any(duplicated(SPDF@data$areaID)) is FALSE
  
  # ----- Add stateCode --------------------------------------------------------
  # TO DO: Needs to have the centroid state calculated
  # Use MazamaSPatialUtils::getStateCode() on polygon centers
  
  # ----- Add country code -----------------------------------------------------
  SPDF$countryCode <- "US"
  
  # ----- Split the primaryLandType field into primaryLandOwner ----------------
  # TO DO: figure out how to make this fit in 80 char
  agency_regexp <- "(?=\\sBIA|\\sBLM|\\sBOR|\\sDOD|\\sFS|\\sFWS|\\sNPS|\\sOTHER|\\sDOE|\\sDOJ|\\sNASA|\\sARS|\\sGSA|\\sDOT|\\sUSDA|\\sCIA)"
  
  SPDF@data <- SPDF@data %>% separate(col = "primaryLandType",
                                      c("primaryLandType", "primaryLandOwner"),
                                      sep = agency_regexp)
  # Remove the space left in primaryLandOwner field
  SPDF@data$primaryLandOwner <- stringr::str_replace(SPDF@data$primaryLandOwner, 
                                                     ' ', 
                                                     '')
  # This generates 38 "unsplit" records
  # Have a look at the records that aren't splitting correctlty.
  missing_data <- subset(SPDF@data, is.na(primaryLandOwner))
  
  # 
  # # ----- Name and save the data -----------------------------------------------
  # message("Saving full resolution version...\n")
  # assign(datasetName, SPDF)
  # save(list = c(datasetName), file = paste0(dataDir,'/',datasetName,'.RData'))
  # rm(list = datasetName)
  # 
  # # ----- Simplify -------------------------------------------------------------
  # 
  # if ( simplify ) {
  #   # Create new, simplified datsets: one with 5%, 2%, and one with 1% of the vertices of the original
  #   # NOTE:  This may take several minutes.
  #   message("Simplifying to 5%...\n")
  #   SPDF_05 <- rmapshaper::ms_simplify(SPDF, 0.05)
  #   SPDF_05@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
  #   datasetName_05 <- paste0(datasetName, "_05")
  #   message("Saving 5% version...\n")
  #   assign(datasetName_05, SPDF_05)
  #   save(list = datasetName_05, file = paste0(dataDir,"/",datasetName_05, '.RData'))
  #   rm(list = c("SPDF_05",datasetName_05))
  #   
  #   message("Simplifying to 2%...\n")
  #   SPDF_02 <- rmapshaper::ms_simplify(SPDF, 0.02)
  #   SPDF_02@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
  #   datasetName_02 <- paste0(datasetName, "_02")
  #   message("Saving 2% version...\n")
  #   assign(datasetName_02, SPDF_02)
  #   save(list = datasetName_02, file = paste0(dataDir,"/",datasetName_02, '.RData'))
  #   rm(list = c("SPDF_02",datasetName_02))
  #   
  #   message("Simplifying to 1%...\n")
  #   SPDF_01 <- rmapshaper::ms_simplify(SPDF, 0.01)
  #   SPDF_01@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
  #   datasetName_01 <- paste0(datasetName, "_01")
  #   message("Saving 1% version...\n")
  #   assign(datasetName_01, SPDF_01)
  #   save(list = datasetName_01, file = paste0(dataDir,"/",datasetName_01, '.RData'))
  #   rm(list = c("SPDF_01",datasetName_01))
  # }
  # 
  # # ----- Clean up and return --------------------------------------------------
  # 
  # unlink(filePath, force = TRUE)
  # unlink(dsnPath, recursive = TRUE, force = TRUE)
  # 
  # return(invisible(datasetName))
  
}

