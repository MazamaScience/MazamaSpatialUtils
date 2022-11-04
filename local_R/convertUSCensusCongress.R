#' @keywords datagen
#' @export

#' @title Convert US congressional districts shapefile

#' @param nameOnly Logical specifying whether to only return the name without
#' creating the file.
#' @param simplify Logical specifying whether to create "_05", _02" and "_01"
#' versions of the file that are simplified to 5\%, 2\% and 1\%.

#' @description Returns a simple features data frame for US Congressional Districts
#' for the 116th US House of Representatives.

#' @details A US congressional district shapefile is downloaded and converted to
#' a simple features data frame with additional columns of data. The resulting
#' file will be created in the spatial data directory which is set with
#' \code{setSpatialDataDir()}.
#'
#' #' The source data is from 2019.
#'
#' @note From the source documentation:
#'
#' Congressional Districts are the 435 areas from which people are elected to the
#' U.S. House of Representatives. After the apportionment of congressional seats
#' among the states based on census population counts, each state is responsible
#' for establishing congressional districts for the purpose of electing representatives.
#' Each congressional district is to be as equal in population to all other
#' congressional districts in a state as practicable. The 116th Congress is seated
#' from January 2019 to 2021. The cartographic boundary files for the District of
#' Columbia, Puerto Rico, and the Island Areas (American Samoa, Guam, the Commonwealth
#' of the Northern Mariana Islands, and the U.S. Virgin Islands) each contain a
#' single record for the non-voting delegate district in these areas. The boundaries
#' of all other congressional districts are provided to the Census Bureau by the
#' states by May 1, 2018.
#'
#' You can join this file with table data downloaded from American FactFinder by
#' using the AFFGEOID field in the cartographic boundary file.
#'
#' @return Name of the datasetName being created.
#'
#' @references \url{https://www2.census.gov/geo/tiger/GENZ2019/}
#'
#' @seealso setSpatialDataDir

convertUSCensusCongress <- function(
  nameOnly = FALSE,
  simplify = TRUE
) {

  # ----- Setup ----------------------------------------------------------------

  # Use package internal data directory
  dataDir <- getSpatialDataDir()

  # Specify the name of the dataset and file being created
  datasetName <- 'USCensus116thCongress'

  if (nameOnly)
    return(datasetName)

  # ----- Get the data ---------------------------------------------------------

  # Build appropriate request URL
  # NOTE: 500k means resolution level 1:500k.
  # RC Note: cd116 means Congressional District (116th Congress)  -- old url: 'http://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_cd115_500k.zip'
  url <- 'https://www2.census.gov/geo/tiger/GENZ2019/shp/cb_2019_us_cd116_500k.zip'

  filePath <- file.path(dataDir, basename(url))
  utils::download.file(url, filePath)
  # NOTE:  This zip file has no directory so extra subdirectory needs to be created
  utils::unzip(filePath, exdir = file.path(dataDir, 'congress'))

  # ----- Convert to SFDF ------------------------------------------------------

  # Convert shapefile into simple features data frame
  # NOTE:  The 'congress' directory has been created
  dsnPath <- file.path(dataDir,'congress')
  shpName <- 'cb_2019_us_cd116_500k'
  SFDF <- .convertLayer(
    dsn = dsnPath,
    layer = shpName,
    encoding = 'UTF-8'
  )

  # ----- Select useful columns and rename -------------------------------------

  # > dplyr::glimpse(SFDF)
  # Rows: 441
  # Columns: 8
  # $ STATEFP  <chr> "48", "26", "35", "17", "17", "17", "05", "06", "06", "06", …
  # $ CD116FP  <chr> "24", "05", "01", "10", "11", "15", "01", "49", "05", "08", …
  # $ AFFGEOID <chr> "5001600US4824", "5001600US2605", "5001600US3501", "5001600 …
  # $ GEOID    <chr> "4824", "2605", "3501", "1710", "1711", "1715", "0501", "06 …
  # $ LSAD     <chr> "C2", "C2", "C2", "C2", "C2", "C2", "C2", "C2", "C2", "C2", …
  # $ CDSESSN  <chr> "116", "116", "116", "116", "116", "116", "116", "116", "11 …
  # $ ALAND    <chr> "680637123", "6083502244", "11916427486", "777275115", "725 …
  # $ AWATER   <chr> "23057547", "4835444214", "16512286", "31723330", "16209000"…

  # Data Dictionary:
  #   STATEFP -----> stateFIPS: 2-digit FIPS code
  #   CD116FP -----> congressionalDistrictFIPS
  #   AFFGEOID ----> AFFGEOID
  #   GEOID -------> GeoID
  #   LSAD --------> (drop)
  #   CDSESSN ------> (drop) this is the congressional district session number of the datasetName (116 for all)
  #   ALAND -------> landArea: land area (in sq. meters)
  #   AWATER ------> waterArea: water area (in sq. meters)

  # Guarantee that ALAND and AWATER are numeric
  SFDF$ALAND <- as.numeric(SFDF$ALAND)
  SFDF$AWATER <- as.numeric(SFDF$AWATER)

  SFDF$countryCode <- "US"
  SFDF$stateCode <- US_stateFIPSToCode(SFDF$STATEFP)

  # Remove outlying territories
  SFDF <- dplyr::filter(SFDF, .data$stateCode %in% US_52)

  # Create the new dataframe in a specific column order
  SFDF <-
    dplyr::select(
      .data = SFDF,
      countryCode = .data$countryCode,
      stateCode = .data$stateCode,
      stateFIPS = .data$STATEFP,
      congressionalDistrictFIPS = .data$CD116FP,
      landArea = .data$ALAND,
      waterArea = .data$AWATER,
      AFFGEOID = .data$AFFGEOID,
      GeoID = .data$GEOID
    )

  # ----- Clean SFDF -----------------------------------------------------------

  # Group polygons with the same identifier
  SFDF <- organizePolygons(
    SFDF,
    uniqueID = 'GeoID',
    sumColumns = c('landArea', 'waterArea')
  )

  # Clean topology errors
  if ( !cleangeo::clgeo_IsValid(SFDF) ) {
    SFDF <- cleangeo::clgeo_Clean(SFDF)
  }

  # ----- Name and save the data -----------------------------------------------

  # Assign a name and save the data
  message("Saving full resolution version...\n")
  assign(datasetName, SFDF)
  save(list = c(datasetName), file = paste0(dataDir, '/', datasetName, '.rda'))
  rm(list = datasetName)

  # ----- Simplify -------------------------------------------------------------

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












