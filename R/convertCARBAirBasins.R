#' @keywords datagen
#' @importFrom rlang .data
#' @export
#'
#' @title Convert California Air Resources Board basin shapefiles
#'
#' @param nameOnly Logical specifying whether to only return the name without
#' creating the file.
#' @param simplify Logical specifying whether to create "_05", _02" and "_01"
#' versions of the file that are simplified to 5\%, 2\% and 1\%.
#'
#' @description Returns a SpatialPolygonsDataFrame for CARB air basins,
#'
#' @details A California air basin shapefile is downloaded and converted to a
#' SpatialPolygonsDataFrame with additional columns of data. The resulting file
#' will be created in the spatial data directory which is set with
#' \code{setSpatialDataDir()}.
#'
#' The source data is from 2004.
#'
#' @note From the source documentation:
#'
#' The California Air Basins layer is a polygon shapefile coverage representing
#' the 15 California air basins, as defined in state statute and regulation.
#' See the California Health and Safety Code, Section 39606 et seq. and
#' California Code of Regulations, Title 17, Section 60100 et seq. Shapefile
#' coverage. Projection: Teale Albers, NAD83, Clarke 1866.
#'
#' Air Basins are designated pursuant to California statute and regulation.
#' Air Basins identify regions of similar meteorological and geographic
#' conditions and consideration for political boundary lines, and are related
#' to air pollution and its transport.
#'
#' @return Name of the dataset being created.
#'
#' @references \url{https://ww2.arb.ca.gov//ei/gislib/gislib.htm}
#'
#' @seealso setSpatialDataDir
#' @seealso getVariable

convertCARBAirBasins <- function(
  nameOnly = FALSE,
  simplify = TRUE
) {

  # ----- Setup ----------------------------------------------------------------

  # Use package internal data directory
  dataDir <- getSpatialDataDir()

  # Specify the name of the dataset and file being created
  datasetName <- 'CA_AirBasins'

  if (nameOnly)
    return(datasetName)

  # ----- Get the data ---------------------------------------------------------

  # Build appropriate request URL
  url <- 'https://www.arb.ca.gov/ei/gislib/boundaries/ca_air_basins.zip'

  filePath <- file.path(dataDir,basename(url))
  utils::download.file(url, filePath)
  # NOTE:  This zip file has no directory so extra subdirectory needs to be created
  utils::unzip(filePath, exdir = file.path(dataDir, 'ca_air_basins'))

  # ----- Convert to SPDF ------------------------------------------------------

  # Convert shapefile into SpatialPolygonsDataFrame
  # NOTE:  The 'ca_air_basins' directory has been created
  dsnPath <- file.path(dataDir, 'ca_air_basins')
  shpName <- 'CaAirBasin'
  SPDF <- convertLayer(
    dsn = dsnPath,
    layerName = shpName,
    encoding = 'UTF-8'
  )

  # ----- Select useful columns and rename -------------------------------------

  # > dplyr::glimpse(SPDF@data)
  # Observations: 15
  # Variables: 5
  # $ AREA      <dbl> 31839198681, 39557878115, 39178615821, 32161425221, 3446577486, …
  # $ PERIMETER <dbl> 1566653.5, 1279107.6, 1291238.4, 1281190.9, 359048.3, 187668.2, …
  # $ CAABA_    <chr> "2", "3", "4", "5", "6", "7", "8", "10", "16", "17", "18", "20",…
  # $ CAABA_ID  <chr> "2", "1", "3", "4", "5", "6", "7", "9", "15", "16", "17", "22", …
  # $ NAME      <chr> "North Coast", "Northeast Plateau", "Sacramento Valley", "Mounta…

  # Data Dictionary:
  #   AREA --------> (drop)
  #   PERIMETER --------> (drop)
  #   CAABA_ ------> CABBA: unique identifer
  #   CAABA_ID ----> CABBA_ID: alternate unique identifer
  #   NAME --------> name: name of the air basin

  # Add core metadata
  SPDF@data$countryCode <- "US"
  SPDF@data$stateCode <- "CA"

  # Create the new dataframe in a specific column order
  SPDF@data <-
    dplyr::select(
      .data = SPDF@data,
      countryCode = .data$countryCode,
      stateCode = .data$stateCode,
      name = .data$NAME,
      CAABA_ = .data$CAABA_,
      CAABA_ID = .data$CAABA_ID
    )

  # ----- Clean SPDF -----------------------------------------------------------

  # Group polygons with the same identifier (CAABA_ID)
  SPDF <- organizePolygons(
    SPDF,
    uniqueID = "CAABA_ID",
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
  save(list = c(datasetName), file = paste0(dataDir, '/', datasetName, '.rda'))
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

