#' @keywords datagen
#' @importFrom rlang .data
#' @export
#'
#' @title Convert US county borders shapefile
#'
#' @param nameOnly Logical specifying whether to only return the name without
#' creating the file.
#' @param simplify Logical specifying whether to create "_05", _02" and "_01"
#' versions of the file that are simplified to 5\%, 2\% and 1\%.
#'
#' @description Create a SpatialPolygonsDataFrame for US counties.
#'
#' @details A US county borders shapefile is downloaded and converted to a
#' SpatialPolygonsDataFrame with additional columns of data. The resulting file
#' will be created in the spatial data directory which is set with
#' \code{setSpatialDataDir()}.
#'
#' The source data is from 2019.
#'
#' @note From the source documentation:
#'
#' The primary legal divisions of most states are termed counties. In Louisiana,
#' these divisions are known as parishes. In Alaska, which has no counties, the
#' equivalent entities are the organized boroughs, city and boroughs,
#' municipalities, and for the unorganized area, census areas. The latter are
#' delineated cooperatively for statistical purposes by the State of Alaska and
#' the Census Bureau. In four states (Maryland, Missouri, Nevada, and Virginia),
#' there are one or more incorporated places that are independent of any county
#' organization and thus constitute primary divisions of their states. These
#' incorporated places are known as independent cities and are treated as
#' equivalent entities for purposes of data presentation. The District of
#' Columbia and Guam have no primary divisions, and each area is considered an
#' equivalent entity for purposes of data presentation. The Census Bureau treats
#' the following entities as equivalents of counties for purposes of data
#' presentation: Municipios in Puerto Rico, Districts and Islands in American
#' Samoa, Municipalities in the Commonwealth of the Northern Mariana Islands,
#' and Islands in the U.S. Virgin Islands. The entire area of the United States,
#' Puerto Rico, and the Island Areas is covered by counties or equivalent entities.
#'
#' You can join this file with table data downloaded from American FactFinder by
#' using the AFFGEOID field in the cartographic boundary file.
#'
#' @return Name of the dataset being created.
#'
#' @references \url{https://www2.census.gov/geo/tiger/GENZ2019/}
#'
#' @seealso setSpatialDataDir
#' @seealso getUSCounty

convertUSCensusCounties <- function(
  nameOnly = FALSE,
  simplify = TRUE
) {

  # ----- Setup ----------------------------------------------------------------

  # Use package internal data directory
  dataDir <- getSpatialDataDir()

  # Specify the name of the dataset and file being created
  datasetName <- 'USCensusCounties'

  if (nameOnly)
    return(datasetName)

  # ----- Get the data ---------------------------------------------------------

  # Build appropriate request URL
  # NOTE: 500k means resolution level 1:500k.
  url <- 'https://www2.census.gov/geo/tiger/GENZ2019/shp/cb_2019_us_county_500k.zip'

  filePath <- file.path(dataDir,basename(url))
  utils::download.file(url,filePath)
  # NOTE:  This zip file has no directory so extra subdirectory needs to be created
  utils::unzip(filePath, exdir = file.path(dataDir, 'counties'))

  # ----- Convert to SPDF ------------------------------------------------------

  # Convert shapefile into SpatialPolygonsDataFrame
  # NOTE:  The 'counties' directory has been created
  dsnPath <- file.path(dataDir, 'counties')
  shpName <- 'cb_2019_us_county_500k'
  SPDF <- convertLayer(
    dsn = dsnPath,
    layerName = shpName,
    encoding = 'UTF-8'
  )

  # ----- Select useful columns and rename -------------------------------------

  # > dplyr::glimpse(SPDF@data)
  # Observations: 3,233
  # Variables: 9
  # $ STATEFP  <chr> "48", "48", "48", "48", "48", "48", "48", "48", "51", "51", "…
  # $ COUNTYFP <chr> "081", "273", "203", "223", "033", "419", "067", "025", "167"…
  # $ COUNTYNS <chr> "01383826", "01383922", "01383887", "01383897", "01383802", "…
  # $ AFFGEOID <chr> "0500000US48081", "0500000US48273", "0500000US48203", "050000…
  # $ GEOID    <chr> "48081", "48273", "48203", "48223", "48033", "48419", "48067"…
  # $ NAME     <chr> "Coke", "Kleberg", "Harrison", "Hopkins", "Borden", "Shelby",…
  # $ LSAD     <chr> "06", "06", "06", "06", "06", "06", "06", "06", "06", "06", "…
  # $ ALAND    <chr> "2361153195", "2282572445", "2331138836", "1987629163", "2324…
  # $ AWATER   <chr> "42331832", "541041659", "40651525", "65639829", "22297606", …

  # Data Dictionary:
  #   STATEFP -----> stateFIPS: 2-digit FIPS code
  #   COUNTYFP -----> combined with STATEFP to make countyFIPS
  #   COUNTYNS -----> COUNTYNS
  #   AFFGEOID ----> (drop)
  #   GEOID -------> (drop)
  #   NAME --------> countyName: English language name
  #   LSAD --------> (drop)
  #   ALAND -------> landArea: land area (in sq. meters)
  #   AWATER ------> waterArea: water area (in sq. meters)

  # Guarantee that ALAND and AWATER are numeric
  SPDF@data$ALAND <- as.numeric(SPDF@data$ALAND)
  SPDF@data$AWATER <- as.numeric(SPDF@data$AWATER)

  SPDF@data$stateCode <- US_stateFIPSToCode(SPDF@data$STATEFP)
  SPDF@data$countryCode <- "US"
  SPDF@data$countyFIPS <- paste0(SPDF@data$STATEFP, SPDF@data$COUNTYFP)

  # Remove outlying territories
  SPDF <- subset(SPDF, SPDF@data$stateCode %in% US_52)

  # Create the new dataframe in a specific column order
  SPDF@data <- dplyr::select(
      .data = SPDF@data,
      countryCode = .data$countryCode,
      stateCode = .data$stateCode,
      stateFIPS = .data$STATEFP,
      countyName = .data$NAME,
      countyFIPS = .data$countyFIPS,
      landArea = .data$ALAND,
      waterArea = .data$AWATER,
      COUNTYNS = .data$COUNTYNS
    )

  # ----- Clean SPDF -----------------------------------------------------------

  # Group polygons with the same identifier (countyName)
  SPDF <- organizePolygons(
    SPDF,
    uniqueID = 'COUNTYNS',
    sumColumns = c('landArea', 'waterArea')
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

