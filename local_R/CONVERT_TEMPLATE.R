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
#' @return Name of the datasetName being created.
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
#   dsnPath <- file.path(dataDir,'ca_air_basins')
#   shpName <- 'CaAirBasin'
#   SFDF <- .convertLayer(dsn = dsnPath, layer = shpName)
#
#   # Fix potentially bad topology first.
#   # https://postgis.net/workshops/postgis-intro/validity.html#st-buffer
#   SFDF <- suppressWarnings( rgeos::gBuffer(SFDF, byid = TRUE, width = 0) )
#   
#   # ----- Select useful columns and rename -------------------------------------
# 
#   # NOTE:  Convert names to human readable, unabbreviated, lowerCamelCase names.
#   # NOTE:  Reasonable names will be constructed as "subTypeNoun" e.g.:
#   # NOTE:    waterSurfaceArea, landSurfaceArea, k12StudentCount, collegeStudentCount
# 
#   # > dplyr::glimpse(SFDF)
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
#   # NOTE:  Some datasetNames have a "unique ID" that is duplicated in the data
#   # NOTE:  because the polygons are not properly nested. An example would be
#   # NOTE:  a state datasetName that stored the San Juan Islands as polygons at the
#   # NOTE:  state level rather then nesting them udner the "Washington" polygon.
#   # NOTE:  In this case the "state" identifier would be duplicated in 
#   # NOTE:  SFDF.
#   # NOTE: 
#   # NOTE:  In these cases, the polygons and associated dataframe need to be 
#   # NOTE:  reorganized. These cases can be identified by checking:
#   # NOTE:
#   # NOTE:    any(duplicated(SFDF[[UNIQUE_ID]]))
#   # NOTE:
#   # NOTE:  This is done with organizePolygons().
#   
#   # TODO: Example from convertUSCensusStates.R
#
#   # Group polygons with the same identifier (stateFIPS)
#   SFDF <- organizePolygons(
#     SFDF, 
#     uniqueID = 'stateFIPS', 
#     sumColumns = c('areaLand', 'areaWater')
#   )
#   
#   # NOTE:  The *cleangeo* package will clean up any topological errors that 
#   # NOTE:  might creep in. These may not be seen until the resulting SFDF
#   # NOTE:  is converted and plotted using the *tmap* package.
#
#   # Clean topology errors
#   if ( !cleangeo::clgeo_IsValid(SFDF) ) {
#     SFDF <- cleangeo::clgeo_Clean(SFDF)
#   }
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
#   # ----- Simplify -------------------------------------------------------------
# 
#   if ( simplify ) {
#     # Create new, simplified datsets: one with 5%, 2%, and one with 1% of the vertices of the original
#     # NOTE:  This may take several minutes.
#     message("Simplifying to 5%...\n")
#     SFDF_05 <- rmapshaper::ms_simplify(SFDF, 0.05)
#     SFDF_05@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
#     datasetName_05 <- paste0(datasetName, "_05")
#     message("Saving 5% version...\n")
#     assign(datasetName_05, SFDF_05)
#     save(list = datasetName_05, file = paste0(dataDir,"/",datasetName_05, '.RData'))
#     rm(list = c("SFDF_05",datasetName_05))
# 
#     message("Simplifying to 2%...\n")
#     SFDF_02 <- rmapshaper::ms_simplify(SFDF, 0.02)
#     SFDF_02@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
#     datasetName_02 <- paste0(datasetName, "_02")
#     message("Saving 2% version...\n")
#     assign(datasetName_02, SFDF_02)
#     save(list = datasetName_02, file = paste0(dataDir,"/",datasetName_02, '.RData'))
#     rm(list = c("SFDF_02",datasetName_02))
# 
#     message("Simplifying to 1%...\n")
#     SFDF_01 <- rmapshaper::ms_simplify(SFDF, 0.01)
#     SFDF_01@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
#     datasetName_01 <- paste0(datasetName, "_01")
#     message("Saving 1% version...\n")
#     assign(datasetName_01, SFDF_01)
#     save(list = datasetName_01, file = paste0(dataDir,"/",datasetName_01, '.RData'))
#     rm(list = c("SFDF_01",datasetName_01))
#   }
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
