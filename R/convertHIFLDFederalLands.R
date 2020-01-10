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

  # ----- Select useful columns and rename -------------------------------------

  # NOTE:  Convert names to human readable, unabbreviated, lowerCamelCase names.
  # NOTE:  Reasonable names will be constructed as "subTypeNoun" e.g.:
  # NOTE:    waterSurfaceArea, landSurfaceArea, k12StudentCount, collegeStudentCount

  # ORIGINAL DATA FIELDS
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
  # 3 Mormon Mountains Wilderness Study Area  <NA>  <NA>    NV         32 1.09456795 0.0708952214 1.09456795
  # 4                                   <NA>  <NA>  <NA>  <NA>       <NA> 0.48873716 0.0039936604 0.48873716
  
  # > names(SPDF@data)
  # [1] "FID"        "AREA"       "PERIMETER"  "FEDLANP020" "FEATURE1"   "FEATURE2"   "FEATURE3"   "AGBUR"      "URL"        "NAME1"      "NAME2"      "NAME3"      "STATE"      "STATE_FIPS" "Shape_Leng"
  # [16] "SHAPE__Are" "SHAPE__Len"

  #
  # SPDF@data <- dplyr::select(
  #   SPDF@data,
  #   stateCode = .data$STATE,
  #   weatherForecastOffice = .data$CWA,
  #   zoneNumber = .data$ZONE,
  #   name = .data$NAME,
  #   zoneID = .data$STATE_ZONE,
  #   longitude = .data$LON,
  #   latitude = .data$LAT
  # )

  # ----- Convert to standard (metric) units -----------------------------------

  # NOTE:  Some datasets should have some variables, typically areas, converted
  # NOTE:  to metric m^2.  Other units will be converted on a case-by-case
  # NOTE:  basis depending on the needs/expectations of any potential user
  # NOTE:  community. (judgement call)

  # TODO

  # ----- Organize polygons ----------------------------------------------------

  # NOTE:  Some datasets have a "unique ID" that is duplicated in the data
  # NOTE:  because the polygons are not properly nested. An example would be
  # NOTE:  a state dataset that stored the San Juan Islands as polygons at the
  # NOTE:  state level rather then nesting them udner the "Washington" polygon.
  # NOTE:  In this case the "state" identifier would be duplicated in
  # NOTE:  SPDF@data.
  # NOTE:
  # NOTE:  In these cases, the polygons and associated dataframe need to be
  # NOTE:  reorganized. These cases can be identified by checking:
  # NOTE:
  # NOTE:    any(dupilcated(SPDF@data[[UNIQUE_ID]]))
  # NOTE:
  # NOTE:  This is done with organizePolygons().

  # TODO: Example from convertWeatherZones.R
  #
  # duplicated <- SPDF$zoneID[duplicated(SPDF$zoneID)]
  # SPDF <- organizePolygons(SPDF, "zoneID", sumColumns = c("longitude", "latitude")) # sumColumns included to avoid complaints
  #
  # # Get correct lat/lon centroids for new polygons
  # data <- SPDF@data
  # centroids <- rgeos::gCentroid(subset(SPDF, SPDF$zoneID %in% duplicated), byid=TRUE)
  # for (id in duplicated) {
  #   data[data$zoneID == id,]$longitude <- centroids[id]$x
  #   data[data$zoneID == id,]$latitude <- centroids[id]$y
  # }
  # SPDF@data <- data

  # ----- Add country and state codes ------------------------------------------

  # NOTE:  Several functions allow filtering by countryCode and stateCode as a
  # NOTE:  way of reducing the number of polygons that need to be searched.
  # NOTE:  These variables should be included inevery dataset.
  # NOTE:
  # NOTE:  These might be assumed or read from a column (that might be coded in
  # NOTE:  some other way). Or, you might have to use MazamaSpatialUtils::getStateCode()
  # NOTE:  on the polygon centers.

  SPDF$countryCode <- "US" # TODO?
  SPDF$stateCode <- "CA"   # TODO?

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

