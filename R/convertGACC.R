#' @importFrom rlang .data
#' @export
#'
#' @title Convert Geographic Area Coordination Center shapefile
#'
#' @description Create a simple features data frame for Geographic Area
#' Coordination Centers (GACCs). These are regions defined by the National
#' Interagency Fire Center (NIFC).
#'
#' The full resolution file will be named "GACC.rda". In addition,
#' "_05", _02" and "_01" versions of the file will be created that that are
#' simplified to 5\%, 2\% and 1\%. Simplified versions will greatly improve the
#' speed of both searching and plotting.
#'
#' @details A GACC shapefile is downloaded and converted to a
#' simple features data frame with additional columns of data. The resulting
#' file will be created in the spatial data directory which is set with
#' \code{setSpatialDataDir()}.
#'
#' The source data is from 2020.
#'
#' @note From the source documentation:
#'
#' Although the primary mission of the GACC is logistical coordination, the
#' Center also has support programs in Predictive Services, Intelligence, and in
#' several Center's Fire Information. Predictive Services consists primarily of
#' professional meteorologists who monitor weather and fuel conditions, conduct
#' briefings, produce fire weather related products, liaison with the National
#' Weather Service, and oversee all aspects of the Remote Automated Weather
#' System (RAWS). The Intelligence Section is primarily responsible for
#' collecting and disseminating wildland fire and prescribed fire activity
#' information, monitoring the status of national firefighting resources,
#' maintaining year-to-date and historical fire occurrence data, and managing
#' the Sit Report and ICS-209 programs.
#'
#' In some GACCs, the Predictive Services and Intelligence sections work as one
#' unit called the Predictive Services Group. The Predictive Services and
#' Intelligence Sections, whether separated or combined, work collaboratively
#' producing Weekly, Monthly, and Seasonal Fire Weather/Fire Danger Outlooks.
#' Each Coordination Center provides additional support to their respective
#' geographic area's wildland fire community through training, workshops,
#' special projects, and other tasks. Except for dispatch of air tankers and
#' lead planes based outside the dispatch center responsibility the fire is
#' located in, the GACC does not have initial-attack dispatch responsibilities.
#' The United States and Alaska are divided into 11 Geographic Areas for the
#' purpose of incident management and mobilization of resources (people,
#' aircraft, ground equipment). Within each Area, an interagency Geographic Area
#' Coordinating Group (GACG), made up of Fire Directors from each of the Federal
#' and State land management agencies from within the Area, is established.
#'
#' Working collaboratively, the GACG's mission is to provide leadership and
#' support not only for wildland fire emergencies, but to other emergency
#' incidents (i.e. earthquakes, floods, hurricanes, tornadoes, etc), as
#' necessary. Authority for establishment of the GACG is through departmental
#' policy and interagency agreements. Additional agreements are established with
#' cooperators and other organizations in order to facilitate efficient fire
#' management activities within and adjacent to the Area. A cost-effective
#' sharing of resources among public agencies is a key component of the GACG
#' mission and is expected by the public, Congress, and States. All agencies and
#' geographic areas work together under the auspices and direction of the
#' National Interagency Fire Center (NIFC). The Geographic Area Coordination
#' Centers (GACC) is a result of an interagency agreement established by the
#' respective Geographic Area Coordinating Group. The primary mission of the
#' GACC is to serve Federal and State wildland fire agencies through logistical
#' coordination and mobilization of resources (people, aircraft, ground
#' equipment) throughout the geographical area, and with other geographic areas,
#' as necessary. This is generally done through coordinating the movement of
#' resources between the many Dispatch Centers within the geographic area and,
#' as necessary, with the National Interagency Coordination Center (NICC) when
#' resources are unavailable within the Area or when mobilization support is
#' needed in other geographic areas. As you survey each GACC website, it will
#' become obvious they are technical in design and are primarily for use by
#' local and geographic area wildland fire managers and firefighters. For the
#' general public, the GACC website may not meet your needs. If this is the case,
#' please check out the National Fire News website, provided by the National
#' Interagency Fire Center, and the InciWeb website, provided as a guide to
#' large fire incidents throughout the United States.
#'
#' The National Wildfire Coordinating Group (NWCG) makes no claims, promises, or
#' guarantees about the accuracy, completeness, or adequacy of the content; and
#' expressly disclaims liability for errors and omissions. No warranty of any
#' kind, implied, expressed or statutory is given with respect to the contents.
#'
#' @return Name of the datasetName being created.
#'
#' @references https://hub.arcgis.com/datasets/nifc::national-gacc-boundaries
#'
#' @seealso setSpatialDataDir
#'
convertGACC <- function() {

  # ----- Setup ----------------------------------------------------------------

  # Use package internal data directory
  dataDir <- getSpatialDataDir()

  # Specify the name of the dataset and file being created
  datasetName <- 'GACC'

  # ----- Get the data ---------------------------------------------------------

  # # Build appropriate request URL
  # # NOTE:  The .zip extension returns a shapefile.
  # url <- "https://opendata.arcgis.com/datasetNames/7dc5f4a286bd47e0aaafa0ab05302fe9_0.zip"
  #
  # filePath <- file.path(dataDir,basename(url))
  # utils::download.file(url,filePath)
  # # NOTE:  This zip file has no directory so extra subdirectory needs to be created
  # utils::unzip(filePath,exdir = file.path(dataDir, 'gacc'))

  # NOTE:  This shape file must now be downloaded and unzipped by hand from:
  # NOTE:    https://hub.arcgis.com/datasets/nifc::national-gacc-boundaries/about

  # ----- Convert to SFDF ------------------------------------------------------

  # Convert shapefile into simple features data frame
  # NOTE: Prior to update, it read in as geojson file

  dsnPath <- file.path(dataDir, 'National_GACC_Boundaries')
  shpName <- 'National_GACC_Current'
  SFDF <- convertLayer(
    dsn = dsnPath,
    layer = shpName
  )

  # ----- Select useful columns and rename -------------------------------------

  # > dplyr::glimpse(SFDF, width = 75)
  # Rows: 10
  # Columns: 11
  # $ OBJECTID   <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
  # $ GACCName   <chr> "Alaska Interagency Coordination Center", "Eastern Are…
  # $ GACCUnitID <chr> "USAKACC", "USWIEACC", "USUTGBC", "USCAONCC", "USMTNRC…
  # $ GACCAbbrev <chr> "AICC", "EACC", "GBCC", "ONCC", "NRCC", "NWCC", "RMCC"…
  # $ GACCLocati <chr> "Fairbanks,AK (Fort Wainwright,AK)", "Milwaukee, WI", …
  # $ ContactPho <chr> "907-356-5680", "414-944-3811", "801-531-5320", "530-2…
  # $ Comments   <chr> "Also dispatches Aleutian Islands; Boundaries updated …
  # $ MapMethod  <chr> "MixedMethods", "MixedMethods", "MixedMethods", "Mixed…
  # $ SHAPE_Leng <dbl> 252.07947, 123.22253, 72.08032, 61.11868, 66.64548, 35…
  # $ SHAPE_Area <dbl> 314.10571, 208.80319, 73.74553, 20.60835, 74.53020, 50…
  # $ geometry   <MULTIPOLYGON [°]> MULTIPOLYGON (((-178.824 51..., MULTIPOLYGON (((-71.58…

  # Data Dictionary:
  #   FID --------> FID:
  #   GeometryID -> (drop)
  #   GACCName ---> GACCName: human readable name
  #   GACCUnitID -> unitID: identifier
  #   GACCAbbrev -> abbreviation, GACC_NWCG_Code (to match 2017 GACC datasetName)
  #   GACCLocati -> location: city, state
  #   ContactPho -> contactPhone: contact phone number
  #   Comments ---> comments: update information
  #   MapMethod --> (drop)
  #   SHAPE_Leng -> (drop)
  #   SHAPE_Area -> (drop)

  # NOTE: Matching the data columns from our 2017 requires the following:
  # NOTE:  * create "label"
  # NOTE:  * create GACC_NWCG_Code

  SFDF$countryCode <- 'US'
  SFDF$stateCode <- stringr::str_sub(SFDF$GACCUnitID, 3, 4)

  # Recreate 2017 labels
  labels_2017 <- c(
    "Eastern", "North Ops", "Northwest", "Rocky Mountain", "Southern",
    "Southwest", "South Ops", "Great Basin", "Northern Rockies", "Alaska"
  )
  names(labels_2017) <- c(
    "EACC", "ONCC", "NWCC", "RMCC", "SACC",
    "SWCC", "OSCC", "GBCC", "NRCC", "AICC"
  )
  SFDF$label <- labels_2017[SFDF$GACCAbbrev]
  SFDF$name <- SFDF$GACCName
  SFDF$GACC_NWCG_Code <- SFDF$GACCAbbrev

  SFDF <-
    SFDF %>%
    dplyr::select(
      countryCode = .data$countryCode,
      stateCode = .data$stateCode,
      unitID = .data$GACCUnitID,
      name = .data$name,
      GACCName = .data$GACCName,         # to support systems built with 2017 version
      abbreviation = .data$GACCAbbrev,
      label = .data$label,
      contactPhone = .data$ContactPho,
      comments = .data$Comments,
      GACC_NWCG_Code = .data$GACC_NWCG_Code  # to support systems built with 2017 version
    )

  # ----- Simplify and save ----------------------------------------------------

  uniqueIdentifier <- "unitID"

  simplifyAndSave(
    SFDF = SFDF,
    datasetName = datasetName,
    uniqueIdentifier = uniqueIdentifier,
    dataDir = dataDir
  )

  # ----- Clean up and return --------------------------------------------------

  # NOTE:  The source file was manually downloaded

  # # Clean up
  # unlink(filePath, force = TRUE)
  # unlink(dsnPath, recursive = TRUE, force = TRUE)
  message("You may delete the manually downloaded source data.")

  return(invisible(datasetName))

}

# ===== TEST ===================================================================

if ( FALSE ) {

  library(sf)

  # Look or horizontal lines from polygons that cross the dateline.
  # NOTE:  These are sometimes created by sf::st_make_valid()
  loadSpatialData(datasetName)
  SFDF <- get(paste0(datasetName, ""))
  SFDF_05 <- get(paste0(datasetName, "_05"))
  SFDF_02 <- get(paste0(datasetName, "_02"))
  SFDF_01 <- get(paste0(datasetName, "_01"))

  plot(SFDF_01$geometry)
  dev.off(dev.list()["RStudioGD"])
  plot(SFDF_02$geometry)
  dev.off(dev.list()["RStudioGD"])
  plot(SFDF_05$geometry)
  dev.off(dev.list()["RStudioGD"])
  #plot(SFDF$geometry)

  # Try out getSpatialData()
  lons <- c(-120:-110, 0:10)
  lats <- c(30:40, 30:40)

  df <- getSpatialData(lons, lats, SFDF_01)
  df <- getSpatialData(lons, lats, SFDF_02)
  df <- getSpatialData(lons, lats, SFDF_05)
  df <- getSpatialData(lons, lats, SFDF)

}
