#' @keywords datagen
#' @importFrom rlang .data
#' @export
#'
#' @title Convert USFS Ranger District Shapefile
#'
#' @param nameOnly Logical specifying whether to only return the name without
#' creating the file.
#' @param simplify Logical specifying whether to create "_05", _02" and "_01"
#' versions of the file that are simplified to 5\%, 2\% and 1\%.
#'
#' @description Create a simple features data frame for USFS Ranger Districts.
#'
#' @details A USFS Ranger Districts shapefile is downloaded and converted to a
#' simple features data frame with additional columns of data. The resulting file
#' will be created in the spatial data directory which is set with
#' \code{setSpatialDataDir()}.
#'
#' The source data is from 2015.
#'
#' @note From the source documentation:
#'
#' This data is intended for read-only use. These data were prepared to describe
#' Forest Service administrative area boundaries. The purpose of the data is to
#' provide display, identification, and analysis tools for determining current
#' boundary information for Forest Service managers, GIS Specialists, and
#' others.
#'
#' The Forest Service has multiple types of boundaries represented by different
#' feature classes (layers): Administrative, Ownership and Proclaimed.
#'
#' 1) ADMINISTRATIVE boundaries (e.g. AdministrativeForest and RangerDistrict
#' feature classes) encompass National Forest System lands managed by an
#' administrative unit. These are dynamic layers that should not be considered
#' "legal" boundaries as they are simply intended to identify the specific
#' organizational units that administer areas. As lands are acquired and
#' disposed, the administrative boundaries are adjusted to expand or shrink
#' accordingly. Please note that ranger districts are sub units of National
#' Forests. An administrative forest boundary can contain one or more Proclaimed
#' National Forests, National Grasslands, Purchase Units, Research and
#' Experimental Areas, Land Utilization Projects and various "Other" Areas. If
#' needed, OWNERSHIP boundaries (e.g. BasicOwnership and SurfaceOwnership
#' feature classes) should be reviewed along with these datasetNames to determine
#' parcels that are federally managed within the administrative boundaries.
#'
#' 2) OWNERSHIP boundaries (e.g. BasicOwnership and SurfaceOwnership feature
#' classes) represent parcels that are tied to legal transactions of ownership.
#' These are parcels of Federal land managed by the USDA Forest Service. Please
#' note that the BasicOwnership layer is simply a dissolved version of the
#' SurfaceOwnership layer.
#'
#' 3) PROCLAIMED boundaries (e.g. ProclaimedForest and
#' ProclaimedForest_Grassland) encompass areas of National Forest System land
#' that is set aside and reserved from public domain by executive order or
#' proclamation.
#'
#' @return Name of the datasetName being created.
#'
#' @references \url{'https://data.fs.usda.gov/geodata/edw/edw_resources/meta/S_USA.RangerDistrict.xml'}
#'
#' @seealso setSpatialDataDir
#' @seealso getVariable

convertUSFSRangerDistricts <- function(
  nameOnly = FALSE,
  simplify = TRUE
) {

  # ----- Setup ---------------------------------------------------------------

  # Use package internal data directory
  dataDir <- getSpatialDataDir()

  # Specify the name of the dataset and file being created
  datasetName <- 'USFSRangerDistricts'

  if (nameOnly)
    return(datasetName)

  # ----- Get the data ---------------------------------------------------------

  # Build appropriate request URL
  url <- 'https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.RangerDistrict.zip'

  filePath <- file.path(dataDir, basename(url))
  utils::download.file(url, filePath)
  # NOTE:  This zip file has no directory so extra subdirectory needs to be created
  utils::unzip(filePath, exdir = file.path(dataDir, 'usfs_ranger_districts'))

  # ----- Convert to SFDF ------------------------------------------------------

  # Convert shapefile into simple features data frame
  # NOTE:  The 'usfs_ranger_districts' directory has been created
  dsnPath <- file.path(dataDir, 'usfs_ranger_districts')
  shpName <- 'S_USA.RangerDistrict'
  SFDF <- convertLayer(
    dsn = dsnPath,
    layer = shpName,
    encoding = 'UTF-8'
  )

  # ----- Select useful columns and rename -------------------------------------

  message("Harmonizing @data...\n")

  # > dplyr::glimpse(SFDF)
  # Observations: 503
  # Variables: 10
  # $ RANGERDIST <chr> "99011607010343", "99011104010343", "99041308010343", "990...
  # $ REGION     <chr> "01", "01", "04", "04", "04", "04", "04", "04", "04", "04"...
  # $ FORESTNUMB <chr> "16", "11", "13", "13", "13", "13", "14", "14", "01", "01"...
  # $ DISTRICTNU <chr> "07", "04", "08", "02", "06", "07", "05", "01", "03", "01"...
  # $ DISTRICTOR <chr> "011607", "011104", "041308", "041302", "041306", "041307"...
  # $ FORESTNAME <chr> "Lolo National Forest", "Custer Gallatin National Forest",...
  # $ DISTRICTNA <chr> "Superior Ranger District", "Yellowstone Ranger District",...
  # $ GIS_ACRES  <dbl> 517194.7, 794292.4, 328967.0, 802906.8, 1031514.7, 775711....
  # $ SHAPE_AREA <dbl> 0.2487376, 0.3702550, 0.1512235, 0.3671154, 0.4736625, 0.3...
  # $ SHAPE_LEN  <dbl> 6.466946, 7.194111, 5.718734, 9.189447, 4.430959, 5.845956...

  # Data Dictionary:
  #   RANGERDIST --> districtID: unique identifier
  #   REGION ------>  regionID: Forest Service Region ID
  #   FORESTNUMB -->  forestNumber: unique ID assigned to a Forest
  #   DISTRICTNU -->  districtNumber: unique ID assigned to a District
  #   DISTRICTOR -->  districtOrgCode: USFS unique identifier for Ranger District
  #   FORESTNAME -->  forestName: name of the forest
  #   DISTRICTNA -->  districtName: name of Ranger District
  #   GIS_ACRES --->  acres: area of the feature in acres
  #   SHAPE_AREA -->  (drop)
  #   SHAPE_LEN --->  (drop)

  # Create human readable Region Name and allStatesCodes vectors
  region_list <- list(
    "01" = "Northern",
    "02" = "Rocky Mountain",
    "03" = "Southwestern",
    "04" = "Intermountain",
    "05" = "Pacific Southwest",
    "06" = "Pacific Northwest",
    "08" = "Southern",
    "09" = "Eastern",
    "10" = "Alaska"
  )

  # NOTE:    https://www.fs.fed.us/wildflowers/regions/
  allStateCodes <- list(
    "01" = "ID,MT,ND,SD",
    "02" = "CO,KS,NE,SD,WY",
    "03" = "AZ,NM",
    "04" = "ID,NV,UT,WY",
    "05" = "CA,HI",
    "06" = "OR,WA",
    "08" = "AL,AR,FL,GA,KN,LA,MS,NC,OK,SC,TN,TX,VA",
    "09" = "CT,DE,IA,IL,IN,MA,MD,ME,MI,MN,MO,NH,NJ,NY,OH,PA,RI,VT,WI,WV",
    "10" = "AK"
  )

  SFDF$regionName <- unlist(region_list[SFDF$REGION], use.names = FALSE)
  SFDF$allStatesCodes <- unlist(allStateCodes[SFDF$REGION], use.names = FALSE)

  SFDF$countryCode <- "US"

  # Create the new dataframe in a specific column order
  SFDF <-
    dplyr::select(
      .data = SFDF,
      countryCode = .data$countryCode,
      districtID = .data$RANGERDIST,
      regionID = .data$REGION,
      regionName = .data$regionName,
      allStateCodes = .data$allStatesCodes,
      forestNumber = .data$FORESTNUMB,
      districtNumber = .data$DISTRICTNU,
      districtOrgCode = .data$DISTRICTOR,
      forestName = .data$FORESTNAME,
      districtName = .data$DISTRICTNA,
      acres = .data$GIS_ACRES
    )

  # ----- Clean SFDF -----------------------------------------------------------

  # Group polygons with the same identifier (districtID)
  message("Organizing polygons...\n")
  SFDF <- organizePolygons(
    SFDF,
    uniqueID = 'districtID',
    sumColumns = NULL
  )

  # Clean topology errors
  message("Checking for topology errors...\n")
  if ( !cleangeo::clgeo_IsValid(SFDF) ) {
    message("Cleaning topology errors...\n")
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
