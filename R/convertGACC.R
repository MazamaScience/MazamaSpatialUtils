#' @keywords datagen
#' @importFrom rlang .data
#' @export
#'
#' @title Convert Geographic Area Coordination Center shapefile
#'
#' @param nameOnly Logical specifying whether to only return the name without
#' creating the file.
#' @param simplify Logical specifying whether to create "_05", _02" and "_01"
#' versions of the file that are simplified to 5\%, 2\% and 1\%.
#'
#' @description Create a SpatialPolygonsDataFrame for Geographic Area
#' Coordination Centers (GACCs). These are regions defined by the National
#' Interagency Fire Center (NIFC).
#'
#' @details A GACC shapefile is downloaded and converted to a
#' SpatialPolygonsDataFrame with additional columns of data. The resulting
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
#' @return Name of the dataset being created.
#'
#' @references https://hub.arcgis.com/datasets/7dc5f4a286bd47e0aaafa0ab05302fe9_0
#'
#' @seealso setSpatialDataDir
#'
convertGACC <- function(
  nameOnly = FALSE,
  simplify = TRUE
) {

  # ----- Setup ----------------------------------------------------------------

  # Use package internal data directory
  dataDir <- getSpatialDataDir()

  # Specify the name of the dataset and file being created
  datasetName <- 'GACC'

  if (nameOnly)
    return(datasetName)

  # ----- Get the data ---------------------------------------------------------

  # Build appropriate request URL
  # NOTE:  The .zip extension returns a shapefile.
  url <- "https://opendata.arcgis.com/datasets/7dc5f4a286bd47e0aaafa0ab05302fe9_0.zip"

  filePath <- file.path(dataDir,basename(url))
  utils::download.file(url,filePath)
  # NOTE:  This zip file has no directory so extra subdirectory needs to be created
  utils::unzip(filePath,exdir = file.path(dataDir, 'gacc'))

  # ----- Convert to SPDF ------------------------------------------------------

  # Convert shapefile into SpatialPolygonsDataFrame
  # NOTE: Prior to update, it read in as geojson file

  dsnPath <- file.path(dataDir,'gacc')
  shpName <- 'National_GACC_Current_20200226'
  SPDF <- convertLayer(dsn = dsnPath, layerName = shpName)

  # ----- Select useful columns and rename -------------------------------------

  # > dplyr::glimpse(SPDF@data)
  # Observations: 10
  # Variables: 15
  # $ FID        <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
  # $ GeometryID <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
  # $ GACCName   <chr> "Alaska Interagency Coordination Center", "Eastern Area Coo…
  # $ GACCUnitID <chr> "USAKACC", "USWIEACC", "USUTGBC", "USCAONCC", "USMTNRC", "U…
  # $ GACCAbbrev <chr> "AICC", "EACC", "GBCC", "ONCC", "NRCC", "NWCC", "RMCC", "SA…
  # $ GACCLocati <chr> "Fairbanks, AK", "Milwaukee, WI", "Boise, ID", "Redding, CA…
  # $ ContactPho <chr> "907-356-5680", "414-944-3811", "800-844-5497", "530-226-28…
  # $ Comments   <chr> "Also dispatches Aleutian Islands; Boundaries updated to 20…
  # $ DateCurren <chr> "2020/02/26", "2020/02/26", "2020/02/26", "2020/02/26", "20…
  # $ MapMethod  <chr> "MixedMethods", "MixedMethods", "MixedMethods", "MixedMetho…
  # $ PrepLevel  <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0", "0"
  # $ PL_GACC_ID <chr> "ACC", "EACC", "GBC", "ONCC", "NRC", "NWCC", "RMC", "SAC", …
  # $ GlobalID   <chr> "{FF5A2D4B-92DB-4BFE-9F7A-E6BC8A4AA923}", "{67E09F2C-E492-4…
  # $ SHAPE_Leng <dbl> 252.07947, 123.22253, 72.08031, 61.11868, 66.64548, 35.7842…
  # $ SHAPE_Area <dbl> 314.10571, 208.80319, 73.74553, 20.60835, 74.53020, 50.8581…

  # Data Dictionary:
  #   FID --------> FID:
  #   GeometryID -> (drop)
  #   GACCName ---> GACCName: human readable name
  #   GACCUnitID -> unitID: identifier
  #   GACCAbbrev -> abbreviation, GACC_NWCG_Code (to match 2017 GACC dataset)
  #   GACCLocati -> location: city, state
  #   ContactPho -> contactPhone: contact phone number
  #   Comments ---> comments: update information
  #   DateCurren -> lastUpdate: date of last update
  #   MapMethod --> (drop)
  #   PrepLevel --> (drop)
  #   PL_GACC_ID -> PL_GACC_ID
  #   GlobalID ---> (drop)
  #   SHAPE_Leng -> (drop)
  #   SHAPE_Area -> (drop)

  # NOTE: Matching the data columns from our 2017 requires the following:
  # NOTE:  * create "label"
  # NOTE:  * create GACC_NWCG_Code

  SPDF@data$countryCode <- 'US'
  SPDF@data$stateCode <- stringr::str_extract(SPDF$GACCLocati, "[[:upper:]]{2}$")

  # Recreate 2017 labels
  labels_2017 <- c(
    "Eastern", "North Ops", "Northwest", "Rocky Mountain", "Southern",
    "Southwest", "South Ops", "Great Basin", "Northern Rockies", "Alaska"
  )
  names(labels_2017) <- c(
    "EACC", "ONCC", "NWCC", "RMCC", "SACC",
    "SWCC", "OSCC", "GBCC", "NRCC", "AICC"
  )
  SPDF@data$label <- labels_2017[SPDF@data$GACCAbbrev]

  SPDF@data <- dplyr::select(
    .data = SPDF@data,
    countryCode = .data$countryCode,
    stateCode = .data$stateCode,
    unitID = .data$GACCUnitID,
    name = .data$GACCName,
    GACCName = .data$GACCName,         # to support systems built with 2017 version
    abbreviation = .data$GACCAbbrev,
    label = .data$label,
    location = .data$GACCLocati,
    contactPhone = .data$ContactPho,
    comments = .data$Comments,
    GACC_NWCG_Code = .data$GACCAbbrev, # to support systems built with 2017 version
    PL_GACC_ID = .data$PL_GACC_ID,
    lastUpdate = .data$DateCurren
  )

  # ----- Organize polygons ----------------------------------------------------

  # Group polygons with the same identifier (unitID)
  SPDF <- organizePolygons(
    SPDF,
    uniqueID = 'unitID',
    sumColumns = NULL
  )

  # ----- Name and save the data -----------------------------------------------

  # Assign a name and save the data
  message("Saving full resolution version...\n")
  assign(datasetName, SPDF)
  save(list = c(datasetName), file = paste0(dataDir,'/', datasetName, '.rda'))
  rm(list = datasetName)

  # ----- Simplify -------------------------------------------------------------

  # simplify
  if ( simplify ) {
    # Create new, simplified datsets: one with 5%, 2%, and one with 1% of the vertices of the original
    # NOTE:  This may take several minutes.
    message("Simplifying to 5%...\n")
    SPDF_05 <- rmapshaper::ms_simplify(SPDF, 0.05)
    SPDF_05@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
    datasetName_05 <- paste0(datasetName, "_05")
    message("Saving 5% version...\n")
    assign(datasetName_05, SPDF_05)
    save(list = datasetName_05, file = paste0(dataDir,"/",datasetName_05, '.rda'))
    rm(list = c("SPDF_05",datasetName_05))

    message("Simplifying to 2%...\n")
    SPDF_02 <- rmapshaper::ms_simplify(SPDF, 0.02)
    SPDF_02@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
    datasetName_02 <- paste0(datasetName, "_02")
    message("Saving 2% version...\n")
    assign(datasetName_02, SPDF_02)
    save(list = datasetName_02, file = paste0(dataDir,"/",datasetName_02, '.rda'))
    rm(list = c("SPDF_02",datasetName_02))

    message("Simplifying to 1%...\n")
    SPDF_01 <- rmapshaper::ms_simplify(SPDF, 0.01)
    SPDF_01@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
    datasetName_01 <- paste0(datasetName, "_01")
    message("Saving 1% version...\n")
    assign(datasetName_01, SPDF_01)
    save(list = datasetName_01, file = paste0(dataDir,"/",datasetName_01, '.rda'))
    rm(list = c("SPDF_01",datasetName_01))
  }

  # ----- Clean up and return --------------------------------------------------

  # Clean up
  unlink(filePath, force = TRUE)
  unlink(dsnPath, recursive = TRUE, force = TRUE)

  return(invisible(datasetName))

}

