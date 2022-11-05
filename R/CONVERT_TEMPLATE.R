#' @export
#'
#' @title Convert TODO shapefile
#'
#' @param nameOnly Logical specifying whether to only return the name without
#' creating the file.
#' @param simplify Logical specifying whether to create "_05", _02" and "_01"
#' versions of the file that are simplified to 5\%, 2\% and 1\%.
#'
#' @return Name of the dataset being created.
#'
#' @description Returns a simple features data frame for TODO
#'
#' The TODO layer is a polygon shapefile coverage representing TODO.
#'
#' TODO: More details
#'
#' @note TODO: MONTH, YEAR version.
#'
#' @references \url{TODO: DATA_URL}

# convertTODO <- function(
#   nameOnly = FALSE,
#   simplify = TRUE
# ) {
#
#   # ----- Setup ---------------------------------------------------------------
#
#   # Use package internal data directory
#   dataDir <- getSpatialDataDir()
#
#   # NOTE:  Dataset names should be lowerCamelCase with now abbreviations
#   # NOTE:  except for known acronymes.
#
#   # Specify the name of the dataset and file being created
#   datasetName <- "TODO"
#
#   if (nameOnly)
#     return(datasetName)
#
#   # ----- Get the data ---------------------------------------------------------
#
#   # NOTE:  Ideally, data can be downloaded from a spcific URL as a .zip file.
#   # NOTE:  In this case, the following example code is a good template.
#
#   # # Build appropriate request URL
#   # url <- 'https://www.arb.ca.gov/ei/gislib/boundaries/ca_air_basins.zip'
#   #
#   # filePath <- file.path(dataDir,basename(url))
#   # utils::download.file(url,filePath)
#   # # NOTE:  This zip file has no directory so extra subdirectory needs to be created
#   # utils::unzip(filePath,exdir=file.path(dataDir, 'ca_air_basins'))
#
#   # ----- Convert to SFDF ------------------------------------------------------
#
#   # NOTE:  You have to look at the downloaded data to determin shpName
#
#   # Convert shapefile into simple features data frame
#   # NOTE:  The 'world' directory has been created
#   dsnPath <- file.path(dataDir, 'TODO')
#   shpName <- 'TODO'
#   SFDF <- .convertLayer(
#     dsn = dsnPath,
#     layer = shpName
#   )
#
#   # ----- Select useful columns and rename -------------------------------------
#
#   # NOTE:  Convert names to human readable, unabbreviated, lowerCamelCase names.
#   # NOTE:  Reasonable names will be constructed as "subTypeNoun" e.g.:
#   # NOTE:    waterSurfaceArea, landSurfaceArea, k12StudentCount, collegeStudentCount
#
#   # > dplyr::glimpse(SFDF, width = 75)
#   # TODO:  PASTE ORIGINAL RESULTS HERE
#
#   # TODO Example from convertWeatherZones.R
#   #
#   # SFDF <- dplyr::select(
#   #   SFDF,
#   #   stateCode = .data$STATE,
#   #   weatherForecastOffice = .data$CWA,
#   #   zoneNumber = .data$ZONE,
#   #   name = .data$NAME,
#   #   zoneID = .data$STATE_ZONE,
#   #   longitude = .data$LON,
#   #   latitude = .data$LAT
#   # )
#
#   # ----- Convert to standard (metric) units -----------------------------------
#
#   # NOTE:  Some datasetNames should have some variables, typically areas, converted
#   # NOTE:  to metric m^2.  Other units will be converted on a case-by-case
#   # NOTE:  basis depending on the needs/expectations of any potential user
#   # NOTE:  community. (judgement call)
#
#   # TODO
#
#   # ----- Clean SFDF -----------------------------------------------------------
#
#   uniqueIdentifier <- "TODO"
#
#   # Guarantee that all polygons are unique
#   if ( any(duplicated(SFDF[[uniqueIdentifier]])) )
#     stop(sprintf("Column '%s' has multiple records. An organizePolygons() step is needed.", uniqueIdentifier))
#
#   # All polygons are unique so we just add polygonID manually
#   SFDF$polygonID <- as.character(seq_len(nrow(SFDF)))
#
#   # Guarantee that all geometries are valid
#   if ( any(!sf::st_is_valid(SFDF)) )
#     SFDF <- sf::st_make_valid(SFDF)
#
#  # TODO:  Check a simplified version of SFDF with
#  # TODO:    plot(SFDF_01$geometry)
#  # TODO:  To see if there are any horizontal lines for polygons that cross the
#  # TODO:  dateline. If so, you may need to skip geometry repair or only apply
#  # TODO:  apply it to select geometries far away from the dateline.
#  # TODO:  See: convertNaturalEarthAdmn1.R
#
#   # ----- Add country and state codes ------------------------------------------
#
#   # NOTE:  Several functions allow filtering by countryCode and stateCode as a
#   # NOTE:  way of reducing the number of polygons that need to be searched.
#   # NOTE:  These variables should be included inevery datasetName.
#   # NOTE:
#   # NOTE:  These might be assumed or read from a column (that might be coded in
#   # NOTE:  some other way). Or, you might have to use MazamaSpatialUtils::getStateCode()
#   # NOTE:  on the polygon centers.
#
#   SFDF$countryCode <- "US" # TODO?
#   SFDF$stateCode <- "CA"   # TODO?
#
#   # NOTE:  Some datasetNames may also wish to include SFDF$allStateCodes to show
#   # NOTE:  all of the states that overlap with each polygon. An example where
#   # NOTE:  this is done is convertWBDHUC.R. (judgement call)
#
#   # ----- Name and save the data -----------------------------------------------
#
#   message("Saving full resolution version...\n")
#   assign(datasetName, SFDF)
#   save(list = c(datasetName), file = paste0(dataDir,'/',datasetName,'.RData'))
#   rm(list = datasetName)
#
#   * Simplify -----
#
#   # TODO:  makeValid setting depends on how the test plot above went.
#
#   if ( simplify )
#     .simplifyAndSave(SFDF, datasetName, dataDir, makeValid = FALSE) # SEE ABOVE
#
#   # ----- Clean up and return --------------------------------------------------
#
#   unlink(filePath, force = TRUE)
#   unlink(dsnPath, recursive = TRUE, force = TRUE)
#
#   return(invisible(datasetName))
#
# }
#
