#' @keywords datagen
#' @importFrom rlang .data
#' @export
#'
#' @title Convert EPA Region shapefiles
#'
#' @param nameOnly Logical specifying whether to only return the name without
#' creating the file.
#' @param simplify Logical specifying whether to create "_05", _02" and "_01"
#' versions of the file that are simplified to 5\%, 2\% and 1\%.
#'
#' @description Returns a SpatialPolygonsDataFrame for EPA Regions
#'
#' @details An EPA region boundary shapefile is downloaded and converted to a
#' SpatialPolygonsDataFrame with additional columns of data. The resulting file
#' will be created in the spatial data directory which is set with
#' \code{setSpatialDataDir()}.
#'
#' The source data is from 2017.
#'
#' @note From the source documentation:
#'
#' This dataset represents delineated EPA Region boundaries. EPA has ten
#' regional offices across the country, each of which is responsible for several
#' states and in some cases, territories or special environmental programs.
#'
#' This Shared Enterprise Geodata and Services (SEGS)dataset was created by
#' U.S. EPA using 2011 TIGER/Line state boundaries from the U.S. Census Bureau.
#' The core mission of SEGS is to provide a single point of ownership for
#' geospatial datasets that are national in extent and of general use to all
#' EPA users and to make those datasets available through channels that best
#' meet user needs.
#'
#' @return Name of the dataset being created.
#'
#' @references \url{https://www.arcgis.com/home/item.html?id=c670540796584c72b4f59b676ccabe6a}
#'
#' @seealso setSpatialDataDir
#' @seealso getVariable

convertEPARegions <- function(
  nameOnly = FALSE,
  simplify = TRUE
) {

  # ----- Setup ---------------------------------------------------------------

  loadSpatialData("USCensusStates")

  # Use package internal data directory
  dataDir <- getSpatialDataDir()

  # Specify the name of the dataset and file being created
  datasetName <- "EPARegions"

  if (nameOnly)
    return(datasetName)

  # ----- Get the data ---------------------------------------------------------

  # Build appropriate request URL
  url <- 'https://opendata.arcgis.com/datasets/c670540796584c72b4f59b676ccabe6a_3.zip'

  filePath <- file.path(dataDir, basename(url))
  utils::download.file(url, filePath)
  # NOTE:  This zip file has no directory so extra subdirectory needs to be created
  utils::unzip(filePath, exdir = file.path(dataDir, 'epa_regions'))

  # ----- Convert to SPDF ------------------------------------------------------

  # Convert shapefile into SpatialPolygonsDataFrame
  # NOTE:  The 'epa_regions' directory has been created
  dsnPath <- file.path(dataDir, 'epa_regions')
  shpName <- 'Environmental_Protection_Agency__EPA__Regions'
  SPDF <- convertLayer(
    dsn = dsnPath,
    layerName = shpName,
    encoding = 'UTF-8'
  )

  # ----- Select useful columns and rename -------------------------------------

  # dplyr::glimpse(SPDF@data)
  #   Observations: 10
  #   Variables: 4
  #   $ OBJECTID   <chr> "1", "2", "3", "4", "5", "6", ...
  #   $ EPAREGION  <chr> "Region 1", "Region 10", "Regi...
  #   $ Shape_Leng <dbl> 33.10624, 298.86497, 40.51849,...
  #   $ Shape_Area <dbl> 20.97047, 389.03109, 19.30733,...

  # Data dictionary:
  #   $ OBJECTID ---> polygonID: unique identifier
  #   $ EPAREGION --> name: the name of the region
  #   $ Shape_Leng --> (drop)
  #   $ Shape_Area --> (drop)

  # Add core metadata
  SPDF@data$countryCode <- "US"

  # Add longitude and latitude as polygon centroids
  centroids <- rgeos::gPointOnSurface(SPDF, byid = TRUE)
  SPDF@data$longitude <- centroids$x
  SPDF@data$latitude <- centroids$y

  # Use longitude and latitude to get one state code for each polygon.
  # NOTE: EPA Regions can span multiple states and include overseas territories
  SPDF@data$stateCode <- getStateCode(
    SPDF@data$longitude,
    SPDF@data$latitude,
    dataset = 'USCensusStates',
    useBuffering = TRUE
  )

  # NOTE:  No good table exists so we create allStateCodes by hand from
  # NOTE:    https://www.epa.gov/aboutepa/visiting-regional-office
  allStateCodes <- list(
    "Region 1" = "CT,MA,ME,NH,RI,VT",
    "Region 2" = "NJ,NY,PR,VI",
    "Region 3" = "DC,DE,MD,PA,VA,WV",
    "Region 4" = "AL,FL,GA,KY,MS,NC,SC,TN",
    "Region 5" = "IL,IN,MI,MN,OH,WI",
    "Region 6" = "AR,LA,NM,OK,TX",
    "Region 7" = "IA,KS,MO,NE",
    "Region 8" = "CO,MT,ND,SD,UT,WY",
    "Region 9" = "AZ,CA,HI,NV",
    "Region 10" = "AK,ID,OR,WA"
  )

  # NOTE:  Super hacky/old school way to get an allStateCodes column
  df <- as.data.frame(as.matrix(allStateCodes))
  SPDF@data$allStateCodes <-
    as.character( df[SPDF@data$EPAREGION,] )

  # Create the new dataframe in a specific column order
  # NOTE: Not adding SPDF$allStateCodes because of overseas territory coverage.
  SPDF@data <- dplyr::select(
    .data = SPDF@data,
    countryCode = .data$countryCode,
    stateCode = .data$stateCode,
    allStateCodes = .data$allStateCodes,
    epaRegion = .data$EPAREGION,
    longitude = .data$longitude,
    latitude = .data$latitude
  )

  # ----- Clean SPDF -----------------------------------------------------------

  # Group polygons with the same identifier (epaRegion)
  SPDF <- organizePolygons(
    SPDF,
    uniqueID = 'epaRegion',
    sumColumns = NULL
  )

  # Clean topology errors
  if ( !cleangeo::clgeo_IsValid(SPDF) ) {
    SPDF <- cleangeo::clgeo_Clean(SPDF)
  }

  # ----- Name and save the data -----------------------------------------------

  # Assign a name and save the data
  message("Saving full resolution version...\n")
  assign(datasetName, SPDF)
  save(list = c(datasetName), file = paste0(dataDir,'/', datasetName, '.rda'))
  rm(list = datasetName)

  # ----- Simplify -------------------------------------------------------------

  if ( simplify ) {
    # Create new, simplified datsets: one with 5%, 2%, and one with 1% of the vertices of the original
    # NOTE:  This may take several minutes.
    message("Simplifying to 5%...\n")
    SPDF_05 <- rmapshaper::ms_simplify(SPDF, 0.05)
    SPDF_05@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
    # Clean topology errors
    if ( !cleangeo::clgeo_IsValid(SPDF_05) ) {
      SPDF_05 <- cleangeo::clgeo_Clean(SPDF_05)
    }
    datasetName_05 <- paste0(datasetName, "_05")
    message("Saving 5% version...\n")
    assign(datasetName_05, SPDF_05)
    save(list = datasetName_05, file = paste0(dataDir,"/", datasetName_05, '.rda'))
    rm(list = c("SPDF_05",datasetName_05))

    message("Simplifying to 2%...\n")
    SPDF_02 <- rmapshaper::ms_simplify(SPDF, 0.02)
    SPDF_02@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
    # Clean topology errors
    if ( !cleangeo::clgeo_IsValid(SPDF_02) ) {
      SPDF_02 <- cleangeo::clgeo_Clean(SPDF_02)
    }
    datasetName_02 <- paste0(datasetName, "_02")
    message("Saving 2% version...\n")
    assign(datasetName_02, SPDF_02)
    save(list = datasetName_02, file = paste0(dataDir,"/", datasetName_02, '.rda'))
    rm(list = c("SPDF_02",datasetName_02))

    message("Simplifying to 1%...\n")
    SPDF_01 <- rmapshaper::ms_simplify(SPDF, 0.01)
    SPDF_01@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
    # Clean topology errors
    if ( !cleangeo::clgeo_IsValid(SPDF_01) ) {
      SPDF_01 <- cleangeo::clgeo_Clean(SPDF_01)
    }
    datasetName_01 <- paste0(datasetName, "_01")
    message("Saving 1% version...\n")
    assign(datasetName_01, SPDF_01)
    save(list = datasetName_01, file = paste0(dataDir,"/", datasetName_01, '.rda'))
    rm(list = c("SPDF_01",datasetName_01))
  }

  # ----- Clean up and return --------------------------------------------------

  # Clean up
  unlink(filePath, force = TRUE)
  unlink(dsnPath, recursive = TRUE, force = TRUE)

  return(invisible(datasetName))

}
