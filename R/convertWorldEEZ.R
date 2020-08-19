#' @keywords datagen
#' @importFrom rlang .data
#' @export
#' 
#' @title Convert World Exclusive Economic Zones shapefile
#' 
#' @param nameOnly logical specifying whether to only return the name without 
#' creating the file
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
  nameOnly = FALSE
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
    #de duplicate concatenated columns
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
  
  # ----- Clean up and return --------------------------------------------------
  
  # Clean up
  unlink(filePath, force = TRUE)
  unlink(dsnPath, recursive = TRUE, force = TRUE)
  
  return(invisible(datasetName))
  
}
