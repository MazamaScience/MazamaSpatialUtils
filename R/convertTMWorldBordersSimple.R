#' @keywords datagen
#' @importFrom rlang .data
#' @export
#'
#' @title Convert (Simple) World Borders Shapefile
#'
#' @param nameOnly Logical specifying whether to only return the name without
#' creating the file.
#' @param simplify Logical specifying whether to create "_05", _02" and "_01"
#' versions of the file that are simplified to 5\%, 2\% and 1\%.
#'
#' @description Returns a SpatialPolygonsDataFrame for simple world divisions
#'
#' @details A world borders shapefile is downloaded and converted to a
#' SpatialPolygonsDataFrame with additional columns of data. The resulting file will be created
#' in the package \code{SpatialDataDir} which is set with \code{setSpatialDataDir()}.
#'
#' This shapefile is a simplified version of the TMWorldBorders shapefile.
#' Users may wish to use a higher resolution dataset when plotting.
#'
#' @return Name of the dataset being created.
#'
#' @references \url{http://thematicmapping.org/downloads/}
#'
#' @seealso setSpatialDataDir
#' @seealso getCountry, getCountryCode

convertTMWorldBordersSimple <- function(
  nameOnly = FALSE,
  simplify = TRUE
) {

  # ----- Setup ----------------------------------------------------------------

  # Use package internal data directory
  dataDir <- getSpatialDataDir()

  # Specify the name of the dataset and file being created
  datasetName <- 'TMWorldBordersSimple'

  if (nameOnly)
    return(datasetName)

  # ----- Get the data ---------------------------------------------------------

  # Build appropriate request URL for TM World Borders data
  url <- 'http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip'

  filePath <- file.path(dataDir,basename(url))
  utils::download.file(url,filePath)
  # NOTE:  This zip file has no directory so extra subdirectory needs to be created
  utils::unzip(filePath,exdir = file.path(dataDir, 'world'))

  # ----- Convert to SPDF ------------------------------------------------------

  # Convert shapefile into SpatialPolygonsDataFrame
  # NOTE:  The 'world' directory has been created
  dsnPath <- file.path(dataDir,'world')
  SPDF <- convertLayer(
    dsn = dsnPath,
    layerName = 'TM_WORLD_BORDERS_SIMPL-0.3',
    encoding = 'UTF-8'
  )

  # ----- Select useful columns and rename -------------------------------------

  #   > dplyr::glimpse(SPDF@data)
  #   Rows: 246
  #   Columns: 11
  #   $ FIPS      <chr> "AC", "AG", "AJ", "AL", "AM", "AO", "AQ", "AR", "AS",  …
  #   $ ISO2      <chr> "AG", "DZ", "AZ", "AL", "AM", "AO", "AS", "AR", "AU",  …
  #   $ ISO3      <chr> "ATG", "DZA", "AZE", "ALB", "ARM", "AGO", "ASM", "ARG",…
  #   $ UN        <int> 28, 12, 31, 8, 51, 24, 16, 32, 36, 48, 52, 60, 44, 50  …
  #   $ NAME      <chr> "Antigua and Barbuda", "Algeria", "Azerbaijan", "Alban …
  #   $ AREA      <int> 44, 238174, 8260, 2740, 2820, 124670, 20, 273669, 7682 …
  #   $ POP2005   <chr> "83039", "32854159", "8352021", "3153731", "3017661",  …
  #   $ REGION    <int> 19, 2, 142, 150, 142, 2, 9, 19, 9, 142, 19, 19, 19, 14 …
  #   $ SUBREGION <int> 29, 15, 145, 39, 145, 17, 61, 5, 53, 145, 29, 21, 29,3 …
  #   $ LON       <dbl> -61.783, 2.632, 47.395, 20.068, 44.563, 17.544, -170.7 …
  #   $ LAT       <dbl> 17.078, 28.163, 40.430, 41.143, 40.534, -12.296, -14.3 …

  # Data Dictionary:
  #   FIPS -----> FIPS: 2-digit FIPS code
  #   ISO2 -----> countryCode
  #   ISO3 ----> ISO3
  #   UN -------> UN_country
  #   NAME ------> countryName
  #   AREA --------> area
  #   POP2005 --------> population2005
  #   REGION -------> UN_region
  #   SUBREGION ------> UN_subregion
  #   LON ------> longitude
  #   LAT ------> latitude

  # Create the new dataframe in a specific column order
  SPDF@data <-
    dplyr::select(
      .data = SPDF@data,
      FIPS = .data$FIPS,
      countryCode = .data$ISO2,
      ISO3 = .data$ISO3,
      UN_country = .data$UN,
      countryName = .data$NAME,
      area = .data$AREA,
      population2005 = .data$POP2005,
      UN_region = .data$REGION,
      UN_subregion = .data$SUBREGION,
      longitude = .data$LON,
      latitude = .data$LAT
    )

  # NOTE:  http://conjugateprior.org/2013/01/unicode-in-r-packages-not/
  # Transliterate unicode characters for this package-internal dataset
  SPDF$countryName <- iconv(SPDF$countryName, from = "UTF-8", to = "ASCII//TRANSLIT")

  # Rationalize units: convert area from units of 10 km^2 to m^2
  SPDF$area <- SPDF$area * 1e7

  # ----- Clean SPDF -----------------------------------------------------------

  # Group polygons with the same identifier (countryCode)
  SPDF <- organizePolygons(
    SPDF,
    uniqueID = 'countryCode',
    sumColumns = c('area', 'population2005')
  )

  # NOTE:  Commented out getting error
  # # Clean topology errors
  # if ( !cleangeo::clgeo_IsValid(SPDF) ) {
  #   SPDF <- cleangeo::clgeo_Clean(SPDF)
  # }

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

