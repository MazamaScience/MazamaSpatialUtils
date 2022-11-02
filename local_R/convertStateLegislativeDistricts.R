#' @keywords datagen
#' @importFrom rlang .data
#' @export
#'
#' @title Convert US state legislative districts shapefile
#'
#' @param stateCode ISO 3166-2 alpha-2 state code.
#' @param house Character specifying either "Upper" or "Lower" house.
#' @param nameOnly Logical specifying whether to only return the name without
#' creating the file.
#' @param simplify Logical specifying whether to create "_05", _02" and "_01"
#' versions of the file that are simplified to 5\%, 2\% and 1\%.
#'
#' @description Create a SpatialPolygonsDataFrame for US State Legislative Districts
#' of a specified state
#'
#' @details A US State Legislative District shapefile is downloaded and converted
#' to a SpatialPolygonsDataFrame with additional columns of data. The resulting
#' file will be created in the spatial data directory which is set with
#' \code{setSpatialDataDir()}.
#'
#' @note From the source documentation:
#'
#' The 2019 cartographic boundary shapefiles are simplified representations of
#' selected geographic areas from the U.S. Census Bureau's Master Address File /
#' Topologically Integrated Geographic Encoding and Referencing (MAF/TIGER)
#' Database (MTDB). These boundary files are specifically designed for small-scale
#' thematic mapping. When possible, generalization is performed with the intent
#' to maintain the hierarchical relationships among geographies and to maintain
#' the alignment of geographies within a file set for a given year. Geographic
#' areas may not align with the same areas from another year. Some geographies
#' are available as nation-based files while others are available only as state-
#' based files.
#'
#' SLDL stands for State Legislative District Lower Chamber...
#'
#' SLDU stands for State Legislative District Upper Chamber.
#'
#' State Legislative Districts (SLDs) are the areas from which members are elected to state
#' legislatures. The SLDs embody the upper (senate) and lower (house) chambers of
#' the state legislature. Nebraska has a unicameral legislature and the District
#' of Columbia has a single council, both of which the Census Bureau treats as
#' upper-chamber legislative areas for the purpose of data presentation; there are
#' no data by SLDL for either Nebraska or the District of Columbia. A unique three-
#' character census code, identified by state participants, is assigned to each
#' SLD within a state. In Connecticut, Illinois, Louisiana, Maine, Maryland,
#' Massachusetts, Michigan, Ohio, and Puerto Rico, the Redistricting Data Program
#' (RDP) participant did not define the SLDs to cover all of the state or state
#' equivalent area. In these areas with no SLDs defined, the code "ZZZ" has been
#' assigned, which is treated as a single SLD for purposes of data presentation.
#'
#' The boundaries of the 2018 state legislative districts were provided by state-
#' level participants through the RDP and reflect the districts used to elect
#' members in or prior to the November 2018 election
#'
#' These files were specifically created to support small-scale thematic mapping.
#' To improve the appearance of shapes at small scales, areas are represented with
#' fewer vertices than detailed TIGER/Line Shapefiles. Cartographic boundary files
#' take up less disk space than their ungeneralized counterparts. Cartographic
#' boundary files take less time to render on screen than TIGER/Line Shapefiles.
#' You can join this file with table data downloaded from American FactFinder by
#' using the AFFGEOID field in the cartographic boundary file. If detailed boundaries
#' are required, please use the TIGER/Line Shapefiles instead of the generalized
#' cartographic boundary files
#'
#' @return Name of the dataset being created.
#'
#' @references \url{https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.html}
#'
#' @seealso setSpatialDataDir

convertStateLegislativeDistricts <- function(
  stateCode,
  house = "Upper",
  nameOnly = FALSE,
  simplify = TRUE
) {

  # ----- Validate Parameters ----------------------------------------------------------------

  # Check that stateCode is a valid state code
  stateCodesDF <- get("US_stateCodes")
  if (!(stateCode %in% stateCodesDF[["stateCode"]])) {
    stop(paste(stateCode, "is not a valid stateCode. Please try again."))
  }

  # Check that house is either "Upper" or "Lower"
  house <-  paste0(toupper(substr(house, 1, 1)), tolower(substring(house, 2)))
  if ( !(house %in% c("Upper", "Lower")) ) {
    stop(paste0("\"",house, "\" is an invalid value for house. Please use either house=\"Upper\" or house=\"Lower\""))
  }

  # Check that house does not equal "Lower" if stateCode equals DC
  if ( (house == "Lower" && stateCode == "DC") ||
       (house == "Lower" && stateCode == "NE") ) {
    warning(paste(stateCode, "does not have a Lower house, converting shapefile for Upper house instead"))
    house <- "Upper"
  }

  # ----- Setup ----------------------------------------------------------------

  # Use package internal data directory
  dataDir <- getSpatialDataDir()

  # Create dataset name
  datasetName <- paste0(stateCode, "_", house, "HouseLegislativeDistricts")

  # Get FIPS code for state
  stateFIPS <- stateCodesDF[stateCodesDF["stateCode"] == stateCode, "stateFIPS"]
  stateFIPS <- stringr::str_replace(stateFIPS, "US", "")  # remove "US" at the beginning of FIPS code

  # create variable to specify Upper or Lower house in request url
  if (house == "Upper") {
    houseChar <- "u"
  } else {  # house == "Lower"
    houseChar <- "l"
  }

  if (nameOnly)
    return(datasetName)

  # ----- Get the data ---------------------------------------------------------

  # build request url
  shpName <- paste0("cb_2019_", stateFIPS, "_sld", houseChar, "_500k")
  url <- paste0("www2.census.gov/geo/tiger/GENZ2019/shp/", shpName, ".zip")

  # download and unzip shapefile
  filePath <- file.path(dataDir, basename(url))
  utils::download.file(url, filePath)
  # NOTE:  This zip file has no directory so extra subdirectory needs to be created
  utils::unzip(filePath, exdir = file.path(dataDir, datasetName))


  # ----- Convert to SPDF ------------------------------------------------------

  # Convert shapefile to SpatialPolygonsDataframe
  dsnPath <- file.path(dataDir, datasetName)
  shpName <- stringr::str_replace(basename(url), ".zip", "")
  SPDF <- convertLayer(
    dsn = dsnPath,
    layerName = shpName,
    encoding = 'UTF-8'
  )

  # ----- Select useful columns and rename -------------------------------------

  message("Harmonizing @data...\n")

  #   > dplyr::glimpse(SPDF@data)
  #   Rows: 35
  #   Columns: 9
  #   $ STATEFP  <chr> "23", "23", "23", "23", "23", "23", "23", "23", "23", "2…
  #   $ SLDUST   <chr> "026", "018", "033", "004", "013", "032", "003", "008", …
  #   $ AFFGEOID <chr> "610U600US23026", "610U600US23018", "610U600US23033", "6…
  #   $ GEOID    <chr> "23026", "23018", "23033", "23004", "23013", "23032", "2…
  #   $ NAME     <chr> "26", "18", "33", "4", "13", "32", "3", "8", "24", "1", …
  #   $ LSAD     <chr> "LU", "LU", "LU", "LU", "LU", "LU", "LU", "LU", "LU", "L…
  #   $ LSY      <chr> "2018", "2018", "2018", "2018", "2018", "2018", "2018", …
  #   $ ALAND    <chr> "535557926", "4537131944", "731235378", "11694167740", "…
  #   $ AWATER   <chr> "115675982", "197779865", "22617661", "1118758210", "629…

  # Data Dictionary:
  #   STATEFP  -----> stateFIPS
  #   SLDUST  -----> (drop)
  #   AFFGEOID -----> (drop)
  #   GEOID    -----> (drop)
  #   NAME    -----> legislativeDistrict
  #   LSAD     -----> legalStatisticalAreaDescriptionCode
  #   LSY      -----> legislativeSessionYear
  #   ALAND    -----> landArea
  #   AWATER   -----> waterArea

  # Add stateCode field
  SPDF@data$stateCode <- stateCode

  # Add countryCode field
  SPDF@data$countryCode <- "US"

  # Ensure that ALAND and ALAND are numeric
  SPDF@data$ALAND <- as.numeric(SPDF@data$ALAND)
  SPDF@data$AWATER <- as.numeric(SPDF@data$AWATER)

  # Create the new dataframe in a specific column order
  SPDF@data <-
    dplyr::select(
      .data = SPDF@data,
      countryCode = .data$countryCode,
      stateCode = .data$stateCode,
      stateFIPS = .data$STATEFP,
      legislativeDistrict = .data$NAME,
      legalStatisticalAreaDescriptionCode = .data$LSAD,
      legislativeSessionYear = .data$LSY,
      landArea = .data$ALAND,
      waterArea = .data$AWATER
    )

  # ----- Clean SPDF -----------------------------------------------------------

  # Group polygons with the same identifier (legislativeDistrict)
  message("Organizing polygons...\n")
  SPDF <- organizePolygons(
    SPDF,
    uniqueID = 'legislativeDistrict',
    sumColumns = c('areaLand', 'areaWater')
  )

  # Clean topology errors
  message("Checking for topology errors...\n")
  if ( !cleangeo::clgeo_IsValid(SPDF) ) {
    message("Cleaning topology errors...\n")
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
