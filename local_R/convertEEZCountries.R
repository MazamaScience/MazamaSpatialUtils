#' @keywords datagen
#' @importFrom rlang .data
#' @export
#'
#' @title Convert Exclusive Economic Zones countries shapefile
#'
#' @param nameOnly Logical specifying whether to only return the name without
#' creating the file.
#' @param simplify Logical specifying whether to create "_05", _02" and "_01"
#' versions of the file that are simplified to 5\%, 2\% and 1\%.
#'
#' @description Create a SpatialPolygonsDataFrame for combined EEZ/country boundaries
#'
#' @details A world EEZ/countries shapefile is converted to a SpatialPolygonsDataFrame
#' with additional columns of data. To use this function, the file
#' "EEZ_land_union_v3_202003.zip" must be downloaded into the user's spatial
#' directory which is set with \code{setSpatialDataDir()}. The resulting file
#' will be created in this same spatial data directory.
#'
#' @note For polygons with overlapping claims of sovereignty, we arbitrarily
#' assign the polygon to the country identified in the \code{ISO_SOV1} field.
#'
#' The source data is from Version 3 -- 2020-03-17.
#'
#' @note From the source documentation:
#'
#' Geographic Information Systems have become indispensable tools in managing
#' and displaying marine data and information. However, a unique georeferenced
#' standard of marine place names and areas was not available, hampering several
#' marine geographic applications, for example the linking of these locations to
#' databases to integrate data. The purpose of Marine Regions is therefore to
#' create a standard, relational list of geographic names, coupled with information
#' and maps of the geographic location of these features. This will improve access
#' and clarity of the different geographic, marine names such as seas, sandbanks,
#' ridges and bays and display univocally the boundaries of marine biogeographic
#' or managerial marine areas.
#'
#' Marine Regions is an integration of the VLIMAR Gazetteer and the VLIZ Maritime
#' Boundaries Geodatabase. The VLIMAR Gazetteer is a database with geographic,
#' mainly marine names such as seas, sandbanks, seamounts, ridges, bays or even
#' standard sampling stations used in marine research. The geographic cover of
#' the VLIMAR gazetteer is global but initially focused on the Belgian Continental
#' Shelf and the Scheldt Estuary and the Southern Bight of the North Sea. Gradually
#' more regional and global geographic information was added to VLIMAR and combining
#' this information with the Maritime Boundaries database, representing the
#' Exclusive Economic Zone (EEZ) of the world, led to the creation of marineregions.org.
#'
#' Marine Regions is managed by the Flanders Marine Institute. Funding for the
#' creation of the VLIMAR gazetteer was provided initially through the EU Network
#' of Excellence MarBEF, but also other European initiatives such as Lifewatch
#' provide the necessary funding for the maintenance and management of Marine Regions.
#'
#' Marine Regions depends on data and knowledge sharing from global, European,
#' regional and national data providers and relevant experts. By setting up
#' Collaboration Agreements, data providers will benefit from belonging to the
#' Marine Regions partnership as they would get increased visibility, gain access
#' to a variety of data analysis services which will benefit from integration of
#' several distributed spatial datasets, as well as enjoying the benefit of the
#' creation of stable unique identifiers. An example template of a Collaboration
#' Agreement can be found here. Please contact info@marineregions.org if your
#' organisation is interested to explore this collaboration.
#'
#' Citation:
#' Flanders Marine Institute (2020). Union of the ESRI Country shapefile and the
#' Exclusive Economic Zones (version 3). Available online at
#' https://www.marineregions.org/. https://doi.org/10.14284/403
#'
#' @return Name of the dataset being created.
#'
#' @references \url{https://www.marineregions.org/sources.php#unioneezcountry}
#'
#' @seealso setSpatialDataDir
#'

convertEEZCountries <- function(
  nameOnly = FALSE,
  simplify = TRUE
) {

  # ----- Setup ----------------------------------------------------------------

  # Use package internal data directory
  dataDir <- getSpatialDataDir()

  # Specify the name of the file being created
  datasetName <- 'EEZCountries'

  if (nameOnly)
    return(datasetName)

  # ----- Get the data ---------------------------------------------------------

  # NOTE:  Downloading the file on a Mac automatically unzips it.

  # # Test if the shapefile directory exists.
  # filePath <- file.path(dataDir,'World_EEZ_v11_20191118_LR.zip')
  # if ( !file.exists(filePath) ) {
  #   stop('Shapefile directory does not exists. Please download and convert the shapefile desired.', call.=FALSE)
  # }
  #
  # # Unzip the downloaded file
  # utils::unzip(filePath, exdir = file.path(dataDir))

  # ----- Convert to SPDF ------------------------------------------------------

  # Convert shapefile into SpatialPolygonsDataFrame
  dsnPath <- file.path(dataDir, 'EEZ_land_union_v3_202003')
  shpName <- "EEZ_Land_v3_202030"
  SPDF <- convertLayer(
    dsn = dsnPath,
    layerName = shpName,
    encoding = 'UTF-8'
  )

  # ----- Select useful columns and rename -------------------------------------


  # > dplyr::glimpse(SPDF@data, width = 80)
  # Rows: 323
  # Columns: 30
  # $ UNION      <chr> "Estonia", "Mayotte", "Overlapping claim Qatar / Saudi Ara…
  # $ MRGID_EEZ  <chr> "5675", "48944", "50170", "8475", "5676", "8340", "8435", …
  # $ TERRITORY1 <chr> "Estonia", "Mayotte", "Qatar", "Cameroon", "Finland", "Bas…
  # $ MRGID_TER1 <chr> "2110", "8606", "8468", "2170", "2106", "31136", "3297", "…
  # $ ISO_TER1   <chr> "EST", "MYT", "QAT", "CMR", "FIN", "ATF", "FRO", "KIR", "C…
  # $ UN_TER1    <chr> "233", "175", "634", "120", "246", "260", "234", "296", "1…
  # $ SOVEREIGN1 <chr> "Estonia", "France", "Qatar", "Cameroon", "Finland", "Fran…
  # $ MRGID_SOV1 <chr> "2110", "17", "8468", "2170", "2106", "17", "2157", "2116"…
  # $ ISO_SOV1   <chr> "EST", "FRA", "QAT", "CMR", "FIN", "FRA", "DNK", "KIR", "C…
  # $ UN_SOV1    <chr> "233", "250", "634", "120", "246", "250", "208", "296", "1…
  # $ TERRITORY2 <chr> NA, "Mayotte", "Saudi Arabia", NA, NA, NA, NA, NA, "Domini…
  # $ MRGID_TER2 <chr> "0", "8606", "2215", "0", "0", "0", "0", "0", "8640", "0",…
  # $ ISO_TER2   <chr> NA, "MYT", "SAU", NA, NA, NA, NA, NA, "DOM", NA, NA, NA, "…
  # $ UN_TER2    <chr> NA, "175", "682", NA, NA, NA, NA, NA, "214", NA, NA, NA, "…
  # $ SOVEREIGN2 <chr> NA, "Comores", "Saudi Arabia", NA, NA, NA, NA, NA, "Domini…
  # $ MRGID_SOV2 <chr> "0", "2163", "2215", "0", "0", "0", "0", "0", "8640", "0",…
  # $ ISO_SOV2   <chr> NA, "COM", "SAU", NA, NA, NA, NA, NA, "DOM", NA, NA, NA, "…
  # $ UN_SOV2    <chr> NA, "174", "682", NA, NA, NA, NA, NA, "214", NA, NA, NA, "…
  # $ TERRITORY3 <chr> NA, NA, "United Arab Emirates", NA, NA, NA, NA, NA, "Venez…
  # $ MRGID_TER3 <chr> "0", "0", "2206", "0", "0", "0", "0", "0", "2201", "0", "0…
  # $ ISO_TER3   <chr> NA, NA, "ARE", NA, NA, NA, NA, NA, "VEN", NA, NA, NA, NA, …
  # $ UN_TER3    <chr> NA, "0", "784", NA, NA, NA, NA, NA, "862", NA, NA, NA, NA,…
  # $ SOVEREIGN3 <chr> NA, NA, "United Arab Emirates", NA, NA, NA, NA, NA, "Venez…
  # $ MRGID_SOV3 <chr> "0", "0", "2206", "0", "0", "0", "0", "0", "2201", "0", "0…
  # $ ISO_SOV3   <chr> NA, NA, "ARE", NA, NA, NA, NA, NA, "VEN", NA, NA, NA, NA, …
  # $ UN_SOV3    <chr> NA, "0", "784", NA, NA, NA, NA, NA, "862", NA, NA, NA, NA,…
  # $ POL_TYPE   <chr> "Union EEZ and country", "Overlapping claim", "Overlapping…
  # $ Y_1        <dbl> 58.71530, -13.21377, 24.69243, 5.62533, 63.97904, -20.8610…
  # $ x_1        <dbl> 24.40898, 45.30528, 51.59986, 12.63665, 25.44114, 39.42539…
  # $ AREA_KM2   <chr> "81842", "67285", "126", "480130", "420076", "120802", "26…

  # Data Dictionary:
  #   UNION ------->
  #   MRGID_EEZ --->
  #   TERRITORY1 -->
  #   MRGID_TER1 -->
  #   ISO_TER1 ---->
  #   UN_TER1 ----->
  #   SOVEREIGN1 --> countryName
  #   MRGID_SOV1 -->
  #   ISO_SOV1 ---->
  #   UN_SOV1 ----->
  #   TERRITORY2 -->
  #   MRGID_TER2 -->
  #   ISO_TER2 ---->
  #   UN_TER2 ----->
  #   SOVEREIGN2 -->
  #   MRGID_SOV2 -->
  #   ISO_SOV2 ---->
  #   UN_SOV2 ----->
  #   TERRITORY3 -->
  #   MRGID_TER3 -->
  #   ISO_TER3 ---->
  #   UN_TER3 ----->
  #   SOVEREIGN3 -->
  #   MRGID_SOV3 -->
  #   ISO_SOV3 ---->
  #   UN_SOV3 ----->
  #   POL_TYPE ---->
  #   Y_1 ---------> latitude
  #   x_1 ---------> longitude
  #   AREA_KM2 ----> area_km2

  oldNames <- names(SPDF@data)
  newNames <- c("countryCode", "countryName", "longitude", "latitude", "area_km2")
  allNames <- c(newNames, oldNames)

  SPDF@data$countryCode <- iso3ToIso2(SPDF@data$ISO_SOV1)
  SPDF@data$countryName <- SPDF@data$SOVEREIGN1
  SPDF@data$longitude <- SPDF@data$x_1
  SPDF@data$latitude <- SPDF@data$Y_1
  SPDF@data$area_km2 <- as.numeric(SPDF@data$AREA_KM2)

  SPDF@data <- SPDF@data[,allNames]

  # ----- Clean SPDF -----------------------------------------------------------

  # NOTE:  All polygons are unique so we just add polygonID manually

  # # Group polygons with the same identifier (stateFIPS)
  # SPDF <- organizePolygons(
  #   SPDF,
  #   uniqueID = 'stateFIPS',
  #   sumColumns = c('landArea', 'waterArea')
  # )

  SPDF@data$polygonID <- as.character(seq_len(nrow(SPDF@data)))

  # Clean topology errors
  if ( !cleangeo::clgeo_IsValid(SPDF) ) {
    SPDF <- cleangeo::clgeo_Clean(SPDF, verbose = TRUE)
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

  # NOTE:  The source file was manually downloaded

  # # Clean up
  # unlink(filePath, force = TRUE)
  # unlink(dsnPath, recursive = TRUE, force = TRUE)
  message("You may delete the manually downloaded source data.")

  return(invisible(datasetName))

}

