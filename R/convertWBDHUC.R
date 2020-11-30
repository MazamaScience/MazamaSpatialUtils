#' @keywords datagen
#' @importFrom rlang .data
#' @export
#'
#' @title Convert USGS hydrologic unit shapefiles
#'
#' @param gdbDir Directory containing the geodatabase.
#' @param level Character or integer which must be 2, 4, 6, 8, 10, 12 or 14.
#' @param nameOnly Logical specifying whether to only return the name without
#' creating the file.
#' @param simplify Logical specifying whether to create "_05", _02" and "_01"
#' versions of the file that are simplified to 5\%, 2\% and 1\%.
#' @param cleanTopology Logical specifying to use the \pkg{cleangeo} package to
#' clean topological errors in the shapefiles.
#'
#' @description Create a SpatialPolygonsDataFrame for USGS watershed boundaries
#'
#' @details A USGS Watershed Boundary Dataset geodatabase is converted to a
#' SpatialPolygonsDataFrame with additional columns of data. To use this
#' function, the WBD geodatabase must be downloaded into a directory which is
#' identified with \code{gdbDir}. The resulting file will be created in the
#' spatial data directory which is set with \code{setSpatialDataDir()}.
#'
#' The full WBD dataset can be downloaded from the USGS with the
#' following command:
#' \preformatted{
#' curl https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/WBD/National/GDB/WBD_National_GDB.zip -O
#' }
#'
#' @note If processing takes too long you can greatly speed things up with:
#' \code{simplify = FALSE, cleanTopology = FALSE}
#'
#' The source data is from Version 2.3 -- 2020-11-19.
#'
#' @note From the source documentation:
#'
#' The Watershed Boundary Dataset (WBD) is a seamless, national hydrologic unit
#' dataset. Simply put, hydrologic units represent the area of the landscape that
#' drains to a portion of the stream network. More specifically, a hydrologic
#' unit defines the areal extent of surface water drainage to an outlet point on
#' a dendritic stream network or to multiple outlet points where the stream
#' network is not dendritic. A hydrologic unit may represent all or only part of
#' the total drainage area to an outlet point so that multiple hydrologic units
#' may be required to define the entire drainage area at a given outlet.
#' Hydrologic unit boundaries in the WBD are determined based on topographic,
#' hydrologic, and other relevant landscape characteristics without regard for
#' administrative, political, or jurisdictional boundaries. The WBD seamlessly
#' represents hydrologic units at six required and two optional hierarchical levels.
#'
#' The hydrologic units (HU) in the WBD form a standardized system for organizing,
#' collecting, managing, and reporting hydrologic information for the nation.
#' The HU in the WBD are arranged in a nested, hierarchical system with each HU
#' in the system identified using a unique code. Hydrologic unit codes (HUC) are
#' developed using a progressive two-digit system where each successively smaller
#' areal unit is identified by adding two digits to the identifying code the
#' smaller unit is nested within. WBD contains eight levels of progressive
#' hydrologic units identified by unique 2- to 16-digit codes. The dataset is
#' complete for the United States to the 12-digit hydrologic unit. The 14- and
#' 16-digit hydrologic units are optional and are not complete for the nation.
#' Efforts are ongoing to complete 10- and 12-digit unit delineations within
#' 8-digit hydrologic units extending across the U.S. – Canada border. Additional
#' information about this effort and access to data is linked on the “resources”
#' section on this page. A similar effort is complete for the 10- and 12-digit
#' units extending across the U.S. – Mexico border.
#'
#' @return Name of the dataset being created.
#'
#' @references \url{https://www.usgs.gov/core-science-systems/ngp/national-hydrography/watershed-boundary-dataset}
#'
#' @seealso setSpatialDataDir

# TODO:  Convert missing state codes to state codes from allStateCodes, with a note explaining
# TODO:  how and why. Figure out why it is printing all those numbers when it runs and change.


convertWBDHUC <- function(
  gdbDir = "~/Data/WBD/WBD_National_GDB.gdb",
  level = 2,
  nameOnly = FALSE,
  simplify = TRUE,
  cleanTopology = FALSE
) {

  # ----- Setup ----------------------------------------------------------------

  # Use package internal data directory
  dataDir <- getSpatialDataDir()

  # 'level' should be a character string
  level <- as.character(level)

  # Specify the name of the dataset and file being created
  datasetName <- paste0('WBDHU', level)

  if (nameOnly)
    return(datasetName)

  # Check existence of NaturalEarthAdm1 needed for assignment of stateCode
  if ( !exists("NaturalEarthAdm1") ) {
    stop("Missing dataset. Please loadSpatialData(\"NaturalEarthAdm1\")",
         call. = FALSE)
  }

  # ----- Get the data ---------------------------------------------------------

  # NOTE:  This 2.2 GB file was downloaded manually and unzipped to create gdbDir

  # Test if the gdb directory exists.
  if ( !dir.exists(gdbDir) ) {
    stop("
  GDB directory does not exists. Please download and unzip the GDB data from:

    https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/WBD/National/GDB/WBD_National_GDB.zip

  Then use the location of WBD_National_GDB.gdb as the 'gdbDir' parameter."
    )
  }

  # ----- Convert to SPDF ------------------------------------------------------

  # > ogrListLayers(gdbDir)
  #  [1] "WBDHU12"                     "NWISDrainageLine"
  #  [3] "WBDHU8"                      "NonContributingDrainageLine"
  #  [5] "WBDHU4"                      "WBDHU16"
  #  [7] "NonContributingDrainageArea" "NWISDrainageArea"
  #  [9] "WBDHU6"                      "WBDHU2"
  # [11] "WBDHU14"                     "WBDLine"
  # [13] "WBDHU10"                     "FeatureToMetadata"
  # [15] "ExternalCrosswalk"           "HUMod"
  # [17] "MetaProcessDetail"           "MetaSourceDetail"
  # [19] "ProcessingParameters"        "UpdateStatus"
  # attr(,"driver")
  # [1] "OpenFileGDB"
  # attr(,"nlayers")
  # [1] 20

  # Convert gdb layer into SpatialPolygonsDataFrame
  dsnPath <- gdbDir
  shpName <- datasetName
  SPDF <- convertLayer(
    dsn = dsnPath,
    layerName = shpName,
    encoding = 'UTF-8'
  )

  # ----- Select useful columns and rename -------------------------------------

  message("Harmonizing @data...\n")

  # NOTE:  Comments are relevant to the WBD as downloaded on 2020-11-20

  if ( level == '2' ) {
    # > pryr::object_size(SPDF)
    # 53.1 MB
    # > dim(SPDF@data)
    # [1] 22 15
    # > names(SPDF@data)
    #  [1] "tnmid"             "metasourceid"      "sourcedatadesc"    "sourceoriginator"
    #  [5] "sourcefeatureid"   "loaddate"          "referencegnis_ids" "areaacres"
    #  [9] "areasqkm"          "states"            "huc2"              "name"
    # [13] "globalid"          "shape_Length"      "shape_Area"
    usefulColumns <- c('loaddate','areasqkm', 'states', 'huc2', 'name')
  } else if ( level == '4' ) {
    # > pryr::object_size(SPDF)
    # 171 MB
    # > dim(SPDF@data)
    # [1] 244  15
    # > names(SPDF@data)
    #  [1] "tnmid"             "metasourceid"      "sourcedatadesc"    "sourceoriginator"
    #  [5] "sourcefeatureid"   "loaddate"          "referencegnis_ids" "areaacres"
    #  [9] "areasqkm"          "states"            "huc4"              "name"
    # [13] "globalid"          "shape_Length"      "shape_Area"
    usefulColumns <- c('loaddate','areasqkm', 'states', 'huc4', 'name')
  } else if ( level == '6' ) {
    # > pryr::object_size(SPDF)
    # 213 MB
    # > dim(SPDF@data)
    # [1] 404  15
    # > names(SPDF@data)
    #  [1] "tnmid"             "metasourceid"      "sourcedatadesc"    "sourceoriginator"
    #  [5] "sourcefeatureid"   "loaddate"          "referencegnis_ids" "areaacres"
    #  [9] "areasqkm"          "states"            "huc6"              "name"
    # [13] "globalid"          "shape_Length"      "shape_Area"
    usefulColumns <- c('loaddate','areasqkm', 'states', 'huc6', 'name')
  } else if (level == '8' ) {
    # > pryr::object_size(SPDF)
    # 459 MB
    # > dim(SPDF@data)
    # [1] 2399   15
    # > names(SPDF@data)
    #  [1] "tnmid"             "metasourceid"      "sourcedatadesc"    "sourceoriginator"
    #  [5] "sourcefeatureid"   "loaddate"          "referencegnis_ids" "areaacres"
    #  [9] "areasqkm"          "states"            "huc8"              "name"
    # [13] "globalid"          "shape_Length"      "shape_Area"
    usefulColumns <- c('loaddate','areasqkm', 'states', 'huc8', 'name')
  } else if (level == '10' ) {
    # > pryr::object_size(SPDF)
    # ???
    # > dim(SPDF@data)
    # ???
    # > names(SPDF@data)
    #  [1] "tnmid"             "metasourceid"      "sourcedatadesc"    "sourceoriginator"
    #  [5] "sourcefeatureid"   "loaddate"          "referencegnis_ids" "areaacres"
    #  [9] "areasqkm"          "states"            "huc10"              "name"
    # [13] "globalid"          "shape_Length"      "shape_Area"
    usefulColumns <- c('loaddate','areasqkm', 'states', 'huc10', 'name')
  } else if (level == '12' ) {
    # > pryr::object_size(SPDF)
    # ???
    # > dim(SPDF@data)
    # ???
    # > names(SPDF@data)
    #  [1] "tnmid"             "metasourceid"      "sourcedatadesc"    "sourceoriginator"
    #  [5] "sourcefeatureid"   "loaddate"          "referencegnis_ids" "areaacres"
    #  [9] "areasqkm"          "states"            "huc12"              "name"
    # [13] "globalid"          "shape_Length"      "shape_Area"
    usefulColumns <- c('loaddate','areasqkm', 'states', 'huc12', 'name')
  } else if (level == '14' ) {
    # > pryr::object_size(SPDF)
    # ???
    # > dim(SPDF@data)
    # ???
    # > names(SPDF@data)
    #  [1] "tnmid"             "metasourceid"      "sourcedatadesc"    "sourceoriginator"
    #  [5] "sourcefeatureid"   "loaddate"          "referencegnis_ids" "areaacres"
    #  [9] "areasqkm"          "states"            "huc14"              "name"
    # [13] "globalid"          "shape_Length"      "shape_Area"
    usefulColumns <- c('loaddate','areasqkm', 'states', 'huc14', 'name')
  }

  # Harmonize names ('huc#' ==> 'HUC')
  SPDF <- SPDF[,usefulColumns]
  names(SPDF) <- c('loadDate', 'area', 'allStateCodes', 'HUC', 'HUCName')

  # Change are from km^2 to m^2
  SPDF@data$area <- as.numeric(SPDF@data$area) * 1e6

  # ----- Clean SPDF -----------------------------------------------------------

  # NOTE:  All polygons are unique so we just add polygonID manually

  # # Group polygons with the same identifier (HUC)
  # message("Organizing polygons...\n")
  # SPDF <- organizePolygons(
  #   SPDF,
  #   uniqueID = 'HUC',
  #   sumColumns = c('area')
  # )

  SPDF@data$polygonID <- SPDF@data$HUC

  if ( cleanTopology ) {

    message("Checking for topology errors...\n")
    # Clean topology errors
    if ( !cleangeo::clgeo_IsValid(SPDF) ) {
      message("Cleaning topology errors...\n")
      SPDF <- cleangeo::clgeo_Clean(SPDF, verbose = TRUE)
    }

  }

  # ----- Add stateCode --------------------------------------------------------

  # TODO:  Larger HUCs are centered in the US, while at smaller levels the entire
  # TODO:  HUCs are in foreign countries (ie Canada). Find a way to eliminate
  # TODO:  smaller HUCs whose 'allStateCodes' is not a US State

  # Calculate centroids to help add more metadata
  result <- try( {
    message("Calculating centroids...\n")
    centroids <- rgeos::gCentroid(SPDF, byid = TRUE)
    lon <- sp::coordinates(centroids)[,1]
    lat <- sp::coordinates(centroids)[,2]
  }, silent = TRUE)

  # NOTE:  This failed for a simplified version of HU10 with:
  # NOTE:
  # NOTE:  Error in createPolygonsComment(p) :
  # NOTE:    rgeos_PolyCreateComment: orphaned hole, cannot find containing polygon for hole at index 147
  # NOTE:
  # NOTE:  If centroids don't work we'll just default to the center of the bbox for each polygon

  if ( "try-error" %in% class(result) ) {
    warning('NOTE: rgeos::gCentroid() failed with the following message. Using bbox() to calculate lon and lat.\n')
    warning(geterrmessage(),'\n')
    lon <- rep(as.numeric(NA), nrow(SPDF))
    lat <- rep(as.numeric(NA), nrow(SPDF))
    for (i in seq_len(nrow(SPDF)) ) {
      bbox <- sp::bbox(SPDF[i,])
      lon[i] <- mean(bbox[1,])
      lat[i] <- mean(bbox[2,])
    }
  }

  # Add more standard columns
  SPDF$longitude <- lon
  SPDF$latitude <- lat
  SPDF$countryCode <- 'US'

  # NOTE:  This takes quite a long time.
  message("Getting stateCode...\n")
  suppressWarnings(SPDF$stateCode <- getStateCode(lon, lat, countryCodes = c('US')))

  # Hack to change missing stateCodes to the value from allStateCodes

  for ( i in seq_len(nrow(SPDF)) ) {
    if ( is.na(SPDF@data$stateCode[i]) ) {
      SPDF@data$stateCode[i] <- SPDF@data$allStateCodes[i]
    }
    # NOTE:  Still NA in the case of level 2 HUC named "United States Minor Outlying Islands"
    if ( !is.na(SPDF@data$stateCode[i]) ) {
      if ( stringr::str_length(SPDF@data$stateCode[i]) > 2 ) {
        SPDF@data$stateCode[i] <- substr(SPDF@data$stateCode[i], start = 1, stop = 2)
      }
    }
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

  # NOTE:  The source file was manually downloaded and is 2.2. GB so don't
  # NOTE:  delete it automatically.

  # # Clean up
  # unlink(filePath, force = TRUE)
  # unlink(dsnPath, recursive = TRUE, force = TRUE)
  message("You may delete the manually downloaded source data.")

  return(invisible(datasetName))

}

# ===== DEBUGGING ==============================================================

# NOTE:  To generate all levels:
if ( FALSE ) {

  for ( i in c(2,4,6) ) {
    message("----- Processing level ", i, " -----\n")
    convertWBDHUC(dsnPath = "~/Data/SpatialRaw/WBD.gdb", level = i, simplify = TRUE)
  }

  # NOTE:  Running out of memory trying to simplify level 8 or above
  for ( i in c(8,10,12) ) {
    message("----- Processing level ", i, " -----\n")
    convertWBDHUC(dsnPath = "~/Data/SpatialRaw/WBD.gdb", level = i, simplify = FALSE)
  }

}

