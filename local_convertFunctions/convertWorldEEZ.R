#' @keywords datagen
#' @importFrom rlang .data
#' @export
#'
#' @title Convert World Exclusive Economic Zones shapefile
#'
#' @param EEZDir Directory containing the unzipped EEZ data.
#' @param nameOnly logical specifying whether to only return the name without
#' creating the file
#' @param simplify Logical specifying whether to create "_05", _02" and "_01"
#' versions of the file that are simplified to 5\%, 2\% and 1\%.
#'
#' @description  Returns a simple features data frame for Exclusive Economic Zones.
#'
#' @details A world EEZ shapefile is converted to a simple features data frame
#' with additional columns of data. To use this function, the file
#' "World_EEZ_v11_20191118.zip" must be downloaded into the users spatial
#' directory and unzipped. The location of the unzipped directory is then set
#' with the \code{EEZDir} parameter. The resulting file
#' will be created in this same spatial data directory.
#'
#' The source data is from Version 11 -- 2019.
#'
#' @note From the source documentation:
#'
#' 1) Maritime Boundaries Geodatabase: Exclusive Economic Zones (200NM), version 11
#'
#' This datasetName builds on previous versions of the world's EEZ. In version 9,
#' the 200 nautical miles outer limit was completely recalculated using a higher
#' resolution coastline as a normal baseline (ESRI Countries 2014) and straight
#' baselines, where available. This datasetName consists of two shapefiles:
#' polylines that represent the maritime boundaries of the world's countries,
#' the other one is a polygon layer representing the Exclusive Economic Zone of
#' countries. This datasetName also contains digital information about treaties,
#' joint regime, and disputed boundaries.
#'
#' Preferred citation:
#'   Flanders Marine Institute (2019). Maritime Boundaries Geodatabase: Maritime
#'   Boundaries and Exclusive Economic Zones (200NM), version 11. Available
#'   online at https://www.marineregions.org/ https://doi.org/10.14284/386.
#'
#' @return Name of the datasetName being created.
#'
#' @references \url{http://www.marineregions.org/downloads.php}
#'
#' @seealso setSpatialDataDir

convertWorldEEZ <- function(
  EEZDir = "~/Data/Spatial/World_EEZ_v11_20191118",
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

  # NOTE:  This 125 MB file was downloaded manually and unzipped to create gdbDir

  # Test if the gdb directory exists.
  if ( !dir.exists(EEZDir) ) {
    stop("
  EEZ directory does not exists. Please download and unzip the EEZ data from:

    http://www.marineregions.org/downloads.php

  Then use the location of World_EEZ_v11_20191118 as the 'EEZDir' parameter."
    )
  }

  # ----- Convert to SFDF ------------------------------------------------------

  # Convert shapefile into simple features data frame
  dsnPath <- EEZDir
  shpName <- "eez_v11"
  SFDF <- .convertLayer(
    dsn = dsnPath,
    layer = shpName,
    encoding = 'UTF-8'
  )

  # ----- Select useful columns and rename -------------------------------------

  # > names(SFDF)
  #  [1] "MRGID"      "GEONAME"    "MRGID_TER1" "POL_TYPE"   "MRGID_SOV1" "TERRITORY1"
  #  [7] "ISO_TER1"   "SOVEREIGN1" "MRGID_TER2" "MRGID_SOV2" "TERRITORY2" "ISO_TER2"
  # [13] "SOVEREIGN2" "MRGID_TER3" "MRGID_SOV3" "TERRITORY3" "ISO_TER3"   "SOVEREIGN3"
  # [19] "X_1"        "Y_1"        "MRGID_EEZ"  "AREA_KM2"   "ISO_SOV1"   "ISO_SOV2"
  # [25] "ISO_SOV3"   "UN_SOV1"    "UN_SOV2"    "UN_SOV3"    "UN_TER1"    "UN_TER2"
  # [31] "UN_TER3"


  ##############################################################################
  # Redo this from here down
  ##############################################################################




  #   NOTE: These column names from a previous version :
  #   > names(SFDF)
  #   [1] "OBJECTID"    "EEZ"        "Country"    "ID"        "Sovereign"  "Remarks"    "Sov_ID"     "EEZ_ID"     "ISO_3digit"  "MRGID"
  #   [11] "Date_chang" "Area_m2"    "Longitude"  "Latitude"  "MRGID_EEZ"

  #   NOTE: this function has changed so the three TERRITORY, SOVEREIGN
  #   NOTE: and ISO_TER columns are concatenated and de-duplicated to create the
  #   NOTE: columns "country", "sovereign", and "ISO3" respectively.

  # Data Dictionary (unlisted column names are dropped and not used):
  #   MRGID  -------> MRGID
  #   GEONAME  -----> EEZ
  #   POL_TYPE -----> polType
  #   TERRITORY1 ---> used to create territory and dropped
  #   ISO_TER1 -----> used to create ISO3 and dropped
  #   SOVEREIGN1 ---> used to create sovereign and dropped
  #   TERRITORY2 ---> used to create territory and dropped
  #   ISO_TER2 -----> used to create ISO3 and dropped
  #   SOVEREIGN2 ---> used to create sovereign and dropped
  #   TERRITORY3 ---> used to create territory and dropped
  #   ISO_TER3 -----> used to create ISO3 and dropped
  #   SOVEREIGN3 ---> used to create sovereign and dropped
  #   X_1 ----------> longitude
  #   Y_1 ----------> latitude
  #   AREA_KM2 -----> area (converted to sq meters by multiplying by 1e6)

  # Create single territory, sovereign, and ISO3 columns and convert area to m
  SFDF <- SFDF %>%
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
      area = .data$AREA_KM2 * 1e6
     )

  # Change missing or multiple ISO3 to NA
  SFDF$ISO3[nchar(SFDF$ISO3) != 3 ] <- NA

  # Add standard columns
  SFDF$countryCode <- iso3ToIso2(SFDF$ISO3)
  SFDF$countryName <- codeToCountry(SFDF$countryCode)

  # Create the new dataframe in a specific column order
  SFDF <-
    dplyr::select(
      .data = SFDF,
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

  # ----- Clean SFDF ----------------------------------------------------

  # Group polygons with the same identifier
  SFDF <- organizePolygons(
    SFDF,
    uniqueID = 'MRGID',
    sumColumns = c('area')
    )

  # Clean topology errors
  if ( !cleangeo::clgeo_IsValid(SFDF) ) {
    SFDF <- cleangeo::clgeo_Clean(SFDF)
  }

  # ----- Name and save the data -----------------------------------------------

  # Assign a name and save the data
  message("Saving full resolution version...\n")
  assign(datasetName, SFDF)
  save(list = c(datasetName), file = paste0(dataDir,'/', datasetName, '.rda'))
  rm(list = datasetName)

  # ----- Simplify -------------------------------------------------------------

  # NOTE: This will probably take an hour or 2
  if ( simplify ) {
    # Create new, simplified datsets: one with 5%, 2%, and one with 1% of the vertices of the original
    # NOTE:  This may take several minutes.
    message("Simplifying to 5%...\n")
    SFDF_05 <- rmapshaper::ms_simplify(SFDF, 0.05)
    SFDF_05@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
    # Clean topology errors
    if ( !cleangeo::clgeo_IsValid(SFDF_05) ) {
      SFDF_05 <- cleangeo::clgeo_Clean(SFDF_05)
    }
    datasetName_05 <- paste0(datasetName, "_05")
    message("Saving 5% version...\n")
    assign(datasetName_05, SFDF_05)
    save(list = datasetName_05, file = paste0(dataDir,"/", datasetName_05, '.rda'))
    rm(list = c("SFDF_05",datasetName_05))

    message("Simplifying to 2%...\n")
    SFDF_02 <- rmapshaper::ms_simplify(SFDF, 0.02)
    SFDF_02@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
    # Clean topology errors
    if ( !cleangeo::clgeo_IsValid(SFDF_02) ) {
      SFDF_02 <- cleangeo::clgeo_Clean(SFDF_02)
    }
    datasetName_02 <- paste0(datasetName, "_02")
    message("Saving 2% version...\n")
    assign(datasetName_02, SFDF_02)
    save(list = datasetName_02, file = paste0(dataDir,"/", datasetName_02, '.rda'))
    rm(list = c("SFDF_02",datasetName_02))

    message("Simplifying to 1%...\n")
    SFDF_01 <- rmapshaper::ms_simplify(SFDF, 0.01)
    SFDF_01@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
    # Clean topology errors
    if ( !cleangeo::clgeo_IsValid(SFDF_01) ) {
      SFDF_01 <- cleangeo::clgeo_Clean(SFDF_01)
    }
    datasetName_01 <- paste0(datasetName, "_01")
    message("Saving 1% version...\n")
    assign(datasetName_01, SFDF_01)
    save(list = datasetName_01, file = paste0(dataDir,"/", datasetName_01, '.rda'))
    rm(list = c("SFDF_01",datasetName_01))
  }

  # ----- Clean up and return --------------------------------------------------

  # Clean up
  unlink(filePath, force = TRUE)
  unlink(dsnPath, recursive = TRUE, force = TRUE)

  return(invisible(datasetName))

}
