#' @importFrom rlang .data
#' @export
#'
#' @title Convert world borders shapefile
#'
#' @param nameOnly Logical specifying whether to only return the name without
#' creating the file.
#' @param simplify Logical specifying whether to create "_05", _02" and "_01"
#' versions of the file that are simplified to 5\%, 2\% and 1\%.
#'
#' @description Returns a simple features data frame for world divisions
#'
#' @details A world borders shapefile is downloaded and converted to a
#' simple features data frame with additional columns of data. The resulting file is
#' created in the spatial data directory which is set with \code{setSpatialDataDir()}.
#'
#' @return Name of the datasetName being created.
#'
#' @references \url{http://thematicmapping.org/downloads/}
#'
#' @seealso setSpatialDataDir
#' @seealso getCountry, getCountryCode

convertTMWorldBorders <- function(
  nameOnly = FALSE,
  simplify = TRUE
) {

  # ----- Setup ----------------------------------------------------------------

  # Use package internal data directory
  dataDir <- getSpatialDataDir()

  # Specify the name of the dataset and file being created
  datasetName <- 'TMWorldBorders'

  if (nameOnly)
    return(datasetName)

  # ----- Get the data ---------------------------------------------------------

  # Build appropriate request URL for TM World Borders data
  url <- "http://thematicmapping.org/downloads/TM_WORLD_BORDERS-0.3.zip"

  filePath <- file.path(dataDir, basename(url))
  utils::download.file(url, filePath)
  # NOTE:  This zip file has no directory so extra subdirectory needs to be created
  utils::unzip(filePath, exdir = file.path(dataDir, 'world'))

  # ----- Convert to SFDF ------------------------------------------------------

  # Convert shapefile into simple features data frame
  # NOTE:  The 'world' directory has been created
  dsnPath <- file.path(dataDir, 'world')
  shpName <- 'TM_WORLD_BORDERS-0.3'
  SFDF <- .convertLayer(
    dsn = dsnPath,
    layer = shpName
  )

  # ----- Select useful columns and rename -------------------------------------

  # > dplyr::glimpse(SFDF, width = 75)
  # Rows: 246
  # Columns: 12
  # $ FIPS      <chr> "AC", "AG", "AJ", "AL", "AM", "AO", "AQ", "AR", "AS", "…
  # $ ISO2      <chr> "AG", "DZ", "AZ", "AL", "AM", "AO", "AS", "AR", "AU", "…
  # $ ISO3      <chr> "ATG", "DZA", "AZE", "ALB", "ARM", "AGO", "ASM", "ARG",…
  # $ UN        <int> 28, 12, 31, 8, 51, 24, 16, 32, 36, 48, 52, 60, 44, 50, …
  # $ NAME      <chr> "Antigua and Barbuda", "Algeria", "Azerbaijan", "Albani…
  # $ AREA      <int> 44, 238174, 8260, 2740, 2820, 124670, 20, 273669, 76823…
  # $ POP2005   <dbl> 83039, 32854159, 8352021, 3153731, 3017661, 16095214, 6…
  # $ REGION    <int> 19, 2, 142, 150, 142, 2, 9, 19, 9, 142, 19, 19, 19, 142…
  # $ SUBREGION <int> 29, 15, 145, 39, 145, 17, 61, 5, 53, 145, 29, 21, 29, 3…
  # $ LON       <dbl> -61.783, 2.632, 47.395, 20.068, 44.563, 17.544, -170.73…
  # $ LAT       <dbl> 17.078, 28.163, 40.430, 41.143, 40.534, -12.296, -14.31…
  # $ geometry  <MULTIPOLYGON [°]> MULTIPOLYGON (((-61.68667 1..., MULTIPOLYG…

  # Data Dictionary:
  #   FIPS ------> FIPS: 2-digit FIPS code
  #   ISO2 ------> countryCode
  #   ISO3 ------> ISO3
  #   UN --------> UN_country
  #   NAME ------> countryName
  #   AREA ------> area
  #   POP2005 ---> population2005
  #   REGION ----> UN_region
  #   SUBREGION -> UN_subregion
  #   LON -------> longitude
  #   LAT -------> latitude


  # Create the new dataframe in a specific column order
  SFDF <-
    dplyr::select(
      .data = SFDF,
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
  # Transliterate unicode characters for this package-internal datasetName
  SFDF$countryName <- iconv(SFDF$countryName, from = "UTF-8", to = "ASCII//TRANSLIT")

  # Rationalize units: convert area from units of 10 km^2 to m^2
  SFDF$area <- SFDF$area * 1e7

  # ----- Clean SFDF -----------------------------------------------------------

  uniqueIdentifier <- "countryCode"

  # Guarantee that all polygons are unique
  if ( any(duplicated(SFDF[[uniqueIdentifier]])) )
    stop(sprintf("Column '%s' has multiple records. An organizePolygons() step is needed.", uniqueIdentifier))

  # All polygons are unique so we just add polygonID manually
  SFDF$polygonID <- as.character(seq_len(nrow(SFDF)))

  # # Guarantee that all geometries are valid
  # if ( any(!sf::st_is_valid(SFDF)) )
  #   SFDF <- sf::st_make_valid(SFDF)

  # NOTE:  If we run the entire thing through sf::st_make_valid(), polygons that
  # NOTE:  cross the dateline get corrupted with big horizontal lines.

  # NOTE: Only 4 polygons are invalid:
  # NOTE:   24 = Canada
  # NOTE:   33 = Chile
  # NOTE:  155 = Norway
  # NOTE:  175 = Russia (obviously stretches across the date line)

  # BOTTOM LINE:  Fix everything but russion
  indicesToFix <- c(24, 33, 155)
  SFDF[indicesToFix,] <- sf::st_make_valid(SFDF[indicesToFix,])

  # ----- Name and save the data -----------------------------------------------

  # Assign a name and save the data
  message("Saving full resolution version...\n")
  assign(datasetName, SFDF)
  save(list = c(datasetName), file = paste0(dataDir, '/', datasetName, '.rda'))
  rm(list = datasetName)

  # * Simplify -----

  if ( simplify )
    .simplifyAndSave(SFDF, datasetName, dataDir, makeValid = FALSE) # SEE ABOVE

  # ----- Clean up and return --------------------------------------------------

  # Clean up
  unlink(filePath, force = TRUE)
  unlink(dsnPath, recursive = TRUE, force = TRUE)

  return(invisible(datasetName))

}

