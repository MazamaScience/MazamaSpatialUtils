#' @keywords datagen
#' @importFrom rlang .data
#' @export
#'
#' @title Convert US Core Based Statistical Areas Shapefile
#'
#' @param nameOnly Logical specifying whether to only return the name without
#' creating the file.
#' @param simplify Logical specifying whether to create "_05", _02" and "_01"
#' versions of the file that are simplified to 5\%, 2\% and 1\%.
#'
#' @description Returns a simple features data frame for US CBSAs
#'
#' @details A US Core Based Statistical Areas (CBSA) shapefile is downloaded and converted to a
#' simple features data frame with additional columns of data. The resulting file
#' will be created in the spatial data directory which is set with
#' \code{setSpatialDataDir()}.
#'
#' @note From the source documentation:
#'
#' Metropolitan and Micropolitan Statistical Areas are together termed Core Based
#' Statistical Areas (CBSAs) and are defined by the Office of Management and Budget
#' (OMB) and consist of the county or counties or equivalent entities associated
#' with at least one urban core (urbanized area or urban cluster) of at least 10,000
#' population, plus adjacent counties having a high degree of social and economic
#' integration with the core as measured through commuting ties with the counties
#' containing the core. Categories of CBSAs are: Metropolitan Statistical Areas,
#' based on urbanized areas of 50,000 or more population; and Micropolitan Statistical
#' Areas, based on urban clusters of at least 10,000 population but less than 50,000
#' population.
#'
#' The CBSA boundaries are those defined by OMB based on the 2010 Census, published
#' in 2013, and updated in 2018
#'
#' @return Name of the datasetName being created.
#'
#' @references \url{https://www2.census.gov/geo/tiger/TIGER2019/CBSA/}
#'
#' @seealso setSpatialDataDir
#' @seealso getUSCounty

convertUSCensusCBSA <- function(
  nameOnly = FALSE,
  simplify = TRUE
) {

  # ----- Setup ----------------------------------------------------------------

  loadSpatialData("USCensusStates")

  # Use package internal data directory
  dataDir <- getSpatialDataDir()

  # Specify the name of the dataset and file being created
  datasetName <- 'USCensusCBSA'

  if (nameOnly)
    return(datasetName)

  # ----- Get the data ---------------------------------------------------------

  # Build appropriate request URL for US County Borders data
  url <- 'https://www2.census.gov/geo/tiger/TIGER2019/CBSA/tl_2019_us_cbsa.zip'

  filePath <- file.path(dataDir, basename(url))
  utils::download.file(url, filePath)
  # NOTE:  This zip file has no directory so extra subdirectory needs to be created
  utils::unzip(filePath, exdir = file.path(dataDir, 'cbsa'))

  # ----- Convert to SFDF ------------------------------------------------------

  # Convert shapefile into simple features data frame
  # NOTE:  The 'cbsa' directory has been created
  dsnPath <- file.path(dataDir,'cbsa')
  shpName <- 'tl_2019_us_cbsa'
  SFDF <- .convertLayer(
    dsn = dsnPath,
    layer = shpName,
    encoding = 'UTF-8'
  )

  # ----- Select useful columns and rename -------------------------------------

  #   > dplyr::glimpse(SFDF)
  #   Rows: 938
  #   Columns: 12
  #   $ CSAFP    <chr> "122", "122", "428", "426", "258", "532", "194", NA, NA, "4 …
  #   $ CBSAFP   <chr> "12020", "12060", "12100", "12120", "12140", "12180", "1222 …
  #   $ GEOID    <chr> "12020", "12060", "12100", "12120", "12140", "12180", "1222 …
  #   $ NAME     <chr> "Athens-Clarke County, GA", "Atlanta-Sandy Springs-Alpharet …
  #   $ NAMELSAD <chr> "Athens-Clarke County, GA Metro Area", "Atlanta-Sandy Spring…
  #   $ LSAD     <chr> "M1", "M1", "M1", "M2", "M2", "M2", "M1", "M1", "M2", "M2", …
  #   $ MEMI     <chr> "1", "1", "1", "2", "2", "2", "1", "1", "2", "2", "1", "2", …
  #   $ MTFCC    <chr> "G3110", "G3110", "G3110", "G3110", "G3110", "G3110", "G311 …
  #   $ ALAND    <chr> "2654601832", "22494938651", "1438776649", "2448115116", "9 …
  #   $ AWATER   <chr> "26140309", "387716575", "301268696", "20504948", "2657419" …
  #   $ INTPTLAT <chr> "+33.9439840", "+33.6937280", "+39.4693555", "+31.1222867", …
  #   $ INTPTLON <chr> "-083.2138965", "-084.3999113", "-074.6337591", "-087.16840 …
  #
  # Data Dictionary:
  #   $ CSAFP  ----->  (drop)
  #   $ CBSAFP  -----> CBSAFP
  #   $ GEOID   -----> (drop)
  #   $ NAME     -----> CBSAName
  #   $ NAMELSAD -----> (drop)
  #   $ LSAD     -----> (drop)
  #   $ MEMI    -----> sizeClass
  #   $ MTFCC   -----> (drop)
  #   $ ALAND    -----> landArea
  #   $ AWATER  -----> waterArea
  #   $ INTPTLAT -----> latitude
  #   $ INTPTLON -----> longitude

  # Convert lat/lon to numeric
  SFDF$INTPTLAT <- as.numeric(SFDF$INTPTLAT)
  SFDF$INTPTLON <- as.numeric(SFDF$INTPTLON)

  # We can use longitude and latitude to get one state code for each polygon.
  # Validation plot -- check if lon/lat are polygon centroids
  if ( FALSE ) {
    tx <- subset(SFDF, stringr::str_detect(SFDF$NAME, "TX"))
    plot(tx)
    points(tx$INTPTLON, tx$INTPTLAT, pch = 16, col = 'red')
  }

  SFDF$stateCode <- getStateCode(SFDF$INTPTLON, SFDF$INTPTLAT, datasetName = 'USCensusStates', useBuffering = TRUE)
  SFDF$countryCode <- "US"

  # Get CBSAName and allStateCodes from the CBSAName column
  nameMatrix <- stringr::str_split_fixed(SFDF$NAME, ',', 2)
  SFDF$CBSAName <- nameMatrix[, 1]
  # allStateCodes is a comma-separate list of stateCodes
  SFDF$allStateCodes <- stringr::str_trim( stringr::str_replace_all(nameMatrix[,2], '-',',') )

  # Convert MEMI to explicitly indicate Micropolitan and Metropolitan classes
  metroMask <- SFDF$MEMI == "1"
  SFDF$MEMI[metroMask] <- "metro"
  SFDF$MEMI[!metroMask] <- "micro"

  # Create the new dataframe in a specific column order
  SFDF <-
    dplyr::select(
      .data = SFDF,
      countryCode = .data$countryCode,
      stateCode = .data$stateCode,
      allStateCodes = .data$allStateCodes,
      CBSAFP = .data$CBSAFP,
      CBSAName = .data$NAME,
      sizeClass = .data$MEMI,
      landArea = .data$ALAND,
      waterArea = .data$AWATER,
      latitude = .data$INTPTLAT,
      longitude = .data$INTPTLON
    )

  # ----- Clean SFDF -----------------------------------------------------------

  # Group polygons with the same identifier (countyName)
  SFDF <- organizePolygons(
    SFDF,
    uniqueID = 'CBSAFP',
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

