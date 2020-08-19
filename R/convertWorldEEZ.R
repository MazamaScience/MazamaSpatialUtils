#' @keywords datagen
#' @importFrom rlang .data
#' @export
#' 
#' @title Convert World Exclusive Economic Zones shapefile
#' 
#' @param nameOnly logical specifying whether to only return the name without 
#' creating the file
#' @param simplify Logical specifying whether to create "_05", _02" and "_01" 
#' versions of the file that are simplified to 5\%, 2\% and 1\%.
#' 
#' @description  Returns a SpatialPolygonsDataFrame for US counties.
#' 
#' @details A world EEZ shapefile is converted to a 
#' SpatialPolygonsDataFrame with additional columns of data. To use this function, 
#' the file World_EEZ_v11_20191118 must be downloaded into the users spatial 
#' directory which is set with \code{setSpatialDataDir()}. The resulting file 
#' will be created in this same spatial data directory.
#' 
#' @return Name of the dataset being created.
#' 
#' @references \url{http://www.marineregions.org/downloads.php}
#' 
#' @seealso setSpatialDataDir
#' @seealso getCountry, getCountryCode
#' 
convertWorldEEZ <- function(
  nameOnly = FALSE,
  simplify = TRUE
) {
  
  # ----- Setup ----------------------------------------------------------------
  
  # Use package internal data directory
  dataDir <- getSpatialDataDir()
  
  # Specify the name of the file being created
  datasetName <- 'WorldEEZ'
  
  if (nameOnly) 
    return(datasetName)
  
  # ----- Get the data ---------------------------------------------------------
  
  # Test if the shapefile directory exists.
  filePath <- file.path(dataDir,'World_EEZ_v11_20191118.zip')
  if ( !file.exists(filePath) ) {
    stop('Shapefile directory does not exists. Please download and convert the shapefile desired.', call.=FALSE)
  } 
  
  # Unzip the downloaded file
  utils::unzip(filePath, exdir = file.path(dataDir))

  # ----- Convert to SPDF ------------------------------------------------------
  
  # Convert shapefile into SpatialPolygonsDataFrame
  dsnPath <- file.path(dataDir, 'World_EEZ_v11_20191118')
  shpName <- "eez_v11"
  SPDF <- convertLayer(
    dsn = dsnPath,
    layerName = shpName,
    encoding = 'UTF-8'
  )
  
  # ----- Select useful columns and rename -------------------------------------
  
  # > names(SPDF)
  # [1] "MRGID"      "GEONAME"    "MRGID_TER1" "POL_TYPE"   "MRGID_SOV1" "TERRITORY1" "ISO_TER1"   "SOVEREIGN1" "MRGID_TER2"
  # [10] "MRGID_SOV2" "TERRITORY2" "ISO_TER2"   "SOVEREIGN2" "MRGID_TER3" "MRGID_SOV3" "TERRITORY3" "ISO_TER3"   "SOVEREIGN3"
  # [19] "X_1"        "Y_1"        "MRGID_EEZ"  "AREA_KM2"   "ISO_SOV1"   "ISO_SOV2"   "ISO_SOV3"   "UN_SOV1"    "UN_SOV2"   
  # [28] "UN_SOV3"    "UN_TER1"    "UN_TER2"    "UN_TER3"   

  #   NOTE: These column names from a previous version :
  #   > names(SPDF)
  #   [1] "OBJECTID"    "EEZ"        "Country"    "ID"        "Sovereign"  "Remarks"    "Sov_ID"     "EEZ_ID"     "ISO_3digit"  "MRGID"
  #   [11] "Date_chang" "Area_m2"    "Longitude"  "Latitude"  "MRGID_EEZ"
  
  #   NOTE: this function has changed so the three TERRITORY, SOVEREIGN 
  #   NOTE: and ISO_TER columns are concatenated and de-duplicated to create the
  #   NOTE: columns "country", "sovereign", and "ISO3" respectively.

  # Data Dictionary (unlisted column names are dropped and not used):
  #   MRGID  -----> MRGID
  #   GEONAME  -----> EEZ
  #   POL_TYPE -----> polType
  #   TERRITORY1 -----> used to create territory and dropped
  #   ISO_TER1 -----> used to create ISO3 and dropped
  #   SOVEREIGN1 -----> used to create sovereign and dropped
  #   TERRITORY2 -----> used to create territory and dropped
  #   ISO_TER2 -----> used to create ISO3 and dropped
  #   SOVEREIGN2 -----> used to create sovereign and dropped
  #   TERRITORY3 -----> used to create territory and dropped
  #   ISO_TER3 -----> used to create ISO3 and dropped
  #   SOVEREIGN3 -----> used to create sovereign and dropped
  #   X_1 -----> longitude
  #   Y_1 -----> latitude
  #   AREA_KM2 -----> area (converted to meters by multiplying my 1000)
  
  SPDF@data <- SPDF@data %>% 
    # concatenate the three TERRITORY, SOVEREIGN and ISO_TER columns
    tidyr::unite("concatTerritory", .data$TERRITORY1, .data$TERRITORY2, .data$TERRITORY3, 
                 sep = '; ', na.rm = TRUE) %>%
    tidyr::unite("concatSovereign", .data$SOVEREIGN1, .data$SOVEREIGN2, .data$SOVEREIGN3, 
                 sep = '; ', na.rm = TRUE) %>%
    tidyr::unite("concatISO", .data$ISO_TER1, .data$ISO_TER2, .data$ISO_TER3, 
                 sep = '; ', na.rm = TRUE) %>%
    #de duplicate concaentated columns
    dplyr::mutate(
      country = vapply(strsplit(.data$concatTerritory, "; "), 
                       function(x) paste(unique(x), collapse = ", "), 
                       character(1)),
      sovereign = vapply(strsplit(.data$concatSovereign, "; "), 
                       function(x) paste(unique(x), collapse = ", "), 
                       character(1)),
      ISO3 = vapply(strsplit(.data$concatISO, "; "), 
                    function(x) paste(unique(x), collapse = ", "), 
                    character(1)),
      area = .data$AREA_KM2 *1000
     ) 
  
  # Change missing or multiple ISO3 to NA
  SPDF@data$ISO3[nchar(SPDF$ISO3) != 3 ] <- NA
  
  # Add standard columns
  SPDF@data$countryCode <- iso3ToIso2(SPDF$ISO3)
  SPDF@data$countryName <- codeToCountry(SPDF$countryCode)
  
  # Create the new dataframe in a specific column order
  SPDF@data <- 
    dplyr::select(
      .data = SPDF@data,
      MRGID = .data$MRGID,
      EEZ = .data$GEONAME,
      countryCode = .data$countryCode,
      countryName = .data$countryName,
      country = .data$country,
      sovereign = .data$sovereign,
      ISO3 = .data$ISO3,
      polType = .data$POL_TYPE,
      longitude = .data$X_1,
      latitude = .data$Y_1,
      area = .data$area,
    )

  # ----- Organize polygons ----------------------------------------------------
  
  # Group polygons with the same identifier
  SPDF <- organizePolygons(
    SPDF, 
    uniqueID = 'MRGID',
    sumColumns = c('area')
    )
  
  # ----- Name and save the data -----------------------------------------------
  
  # Assign a name and save the data
  message("Saving full resolution version...\n")
  assign(datasetName, SPDF)
  save(list = c(datasetName), file = paste0(dataDir,'/', datasetName, '.rda'))
  rm(list = datasetName)
  
  # ----- Simplify -------------------------------------------------------------
 
   # NOTE: using the simplify functionality took over 2 hours maybe this should be removed 
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
  
  # Clean up
  unlink(filePath, force = TRUE)
  unlink(dsnPath, recursive = TRUE, force = TRUE)
  
  return(invisible(datasetName))
  
}
