#' @keywords datagen
#' @export
#' 
#' @title Convert USFS Ranger District shapefiles
#' 
#' @param nameOnly Logical specifying whether to only return the name without 
#' creating the file.
#' @param simplify Logical specifying whether to create "_05", _02" and "_01" 
#' versions of the file that are simplified to 5\%, 2\% and 1\%.
#' 
#' @return Name of the dataset being created.
#' 
#' @description Returns a SpatialPolygonsDataFrame for USFS Ranger Districts
#' 
#' The USFSRangerDistricts layer is a polygon shapefile coverage representing 
#' US Forest Service ranger district administrative boundaries.  Ranger 
#' districts are sub units of National Forests intended to identify the specific 
#' organizational units that administer areas.  
#' 
#' The purpose of the data is to provide display, identification, and analysis 
#' tools for determining current boundary information for Forest Service 
#' managers, GIS Specialists, and others.
#' 
#' See https://data.fs.usda.gov/geodata/edw/edw_resources/meta/S_USA.RangerDistrict.xml
#' for more information on this data set.
#' 
#' @note  Refreshed Feb 3, 2020.
#' 
#' @references \url{'https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.RangerDistrict.zip'}

convertUSFSRangerDistricts <- function(
  nameOnly = FALSE,
  simplify = TRUE
) {
  
  # ----- Setup ---------------------------------------------------------------
  loadSpatialData("USCensusStates")
  
  # Use package internal data directory
  dataDir <- getSpatialDataDir()
  
  # Specify the name of the dataset and file being created
  datasetName <- "USFSRangerDistricts"
  
  if (nameOnly)
    return(datasetName)
  
  # ----- Get the data ---------------------------------------------------------
  
  # Build appropriate request URL
  url <- 'https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.RangerDistrict.zip'
  
  filePath <- file.path(dataDir,basename(url))
  utils::download.file(url,filePath)
  # NOTE:  This zip file has no directory so extra subdirectory needs to be created
  utils::unzip(filePath, exdir = file.path(dataDir, 'usfs_ranger_districts'))
  
  
  # ----- Convert to SPDF ------------------------------------------------------
  
  # Convert shapefile into SpatialPolygonsDataFrame
  dsnPath <- file.path(dataDir,'usfs_ranger_districts')
  shpName <- 'S_USA.RangerDistrict'
  SPDF <- convertLayer(dsn = dsnPath, layerName = shpName)
  
  # Fix potentially bad topology first.
  # https://postgis.net/workshops/postgis-intro/validity.html#st-buffer
  SPDF <- suppressWarnings( rgeos::gBuffer(SPDF, byid = TRUE, width = 0) )
  
  # ----- Select useful columns and rename -------------------------------------
  #   > dplyr::glimpse(SPDF@data)
  #   Observations: 503
  #   Variables: 10
  #   $ RANGERDIST <chr> "99011807010343", "99011501010343", "99011506010343", …
  #   $ REGION     <chr> "01", "01", "01", "01", "01", "01", "01", "01", "04", …
  #   $ FORESTNUMB <chr> "18", "15", "15", "15", "02", "04", "04", "04", "14", …
  #   $ DISTRICTNU <chr> "07", "01", "06", "07", "04", "07", "06", "08", "01", …
  #   $ DISTRICTOR <chr> "011807", "011501", "011506", "011507", "010204", "010…
  #   $ FORESTNAME <chr> "Dakota Prairie Grasslands", "Helena-Lewis and Clark N…
  #   $ DISTRICTNA <chr> "Medora Ranger District", "Rocky Mountain Ranger Distr…
  #   $ GIS_ACRES  <dbl> 1237515.9, 783923.2, 564105.7, 654431.1, 675794.1, 488…
  #   $ SHAPE_AREA <dbl> 0.58961690, 0.38059288, 0.26925323, 0.31210679, 0.3178…
  #   $ SHAPE_LEN  <dbl> 5.420085, 5.049063, 6.209311, 5.896783, 12.349864, 5.5…
  
  # Remapped fields:
  # RANGERDIST --> districtID 
  # REGION     --> regionID: Forest Service Region ID, ie "02"
  #                           01 - Northern
  #                           02 - Rocky Mountain
  #                           03 - Southwestern
  #                           04 - Intermountain
  #                           05 - Pacific Southwest
  #                           06 - Pacific Northwest
  #                           08 - Southern
  #                           09 - Eastern
  #                           10 - Alaska
  # (new)      --> regionName: Forest Service Region name, ie "Rocky Mountain"
  # FORESTNUMB --> forestNumber: ID assigned to a Forest that is unique within a Region.
  # DISTRICTNU --> districtNumber: ID assigned to a District that is unique within a Forest
  # DISTRICTOR --> districtOrgCode: USFS org. code identifying the Ranger District
  # FORESTNAME --> forestName: official name of National Forest (or other Administrative Area).
  # DISTRICTNA --> districtName: official name of Ranger District
  # GIS_ACRES  --> acres: area of the feature in acres
  # SHAPE_AREA --> (drop)
  # SHAPE_LEN  --> (drop)
  
  # Create human readable Region Name vector
  region_list <- list("01" = "Northern",
                      "02" = "Rocky Mountain",
                      "03" = "Southwestern",
                      "04" = "Intermountain",
                      "05" = "Pacific Southwest",
                      "06" = "Pacific Northwest",
                      "08" = "Southern",
                      "09" = "Eastern",
                      "10" = "Alaska")
  region_names <- unlist(region_list[SPDF@data$REGION], use.names = FALSE)
  
  SPDF@data <- dplyr::select(
    SPDF@data,
    districtID = .data$RANGERDIST,
    regionID = .data$REGION,
    forestNumber = .data$FORESTNUMB,
    districtNumber = .data$DISTRICTNU,
    districtOrgCode = .data$DISTRICTOR,
    forestName = .data$FORESTNAME,
    districtName = .data$DISTRICTNA,
    acres = .data$GIS_ACRES
  )
  
  # Assign names to regions
  SPDF@data$regionName <- region_names
  
  # ----- Organize polygons ----------------------------------------------------
  
  #any(duplicated(SPDF@data$districtID))
  SPDF <- organizePolygons(SPDF, "districtID")
  
  # ----- Add country and state codes ------------------------------------------
  
  SPDF$countryCode <- "US"
  
  centroids <- rgeos::gPointOnSurface(SPDF, byid = TRUE)
  SPDF@data$longitude <- centroids$x
  SPDF@data$latitude <- centroids$y
  
  SPDF$stateCode <- getStateCode(
    SPDF@data$longitude, 
    SPDF@data$latitude, 
    dataset = 'USCensusStates', 
    useBuffering = TRUE
  )
  
  
    # ----- Name and save the data -----------------------------------------------

    message("Saving full resolution version...\n")
    assign(datasetName, SPDF)
    save(list = c(datasetName), file = paste0(dataDir,'/',datasetName,'.RData'))
    rm(list = datasetName)

    # ----- Simplify -------------------------------------------------------------

    if ( simplify ) {
      # Create new, simplified datsets: one with 5%, 2%, and one with 1% of the vertices of the original
      # NOTE:  This may take several minutes.
      message("Simplifying to 5%...\n")
      SPDF_05 <- rmapshaper::ms_simplify(SPDF, 0.05)
      SPDF_05@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
      datasetName_05 <- paste0(datasetName, "_05")
      message("Saving 5% version...\n")
      assign(datasetName_05, SPDF_05)
      save(list = datasetName_05, file = paste0(dataDir,"/",datasetName_05, '.RData'))
      rm(list = c("SPDF_05",datasetName_05))

      message("Simplifying to 2%...\n")
      SPDF_02 <- rmapshaper::ms_simplify(SPDF, 0.02)
      SPDF_02@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
      datasetName_02 <- paste0(datasetName, "_02")
      message("Saving 2% version...\n")
      assign(datasetName_02, SPDF_02)
      save(list = datasetName_02, file = paste0(dataDir,"/",datasetName_02, '.RData'))
      rm(list = c("SPDF_02",datasetName_02))

      message("Simplifying to 1%...\n")
      SPDF_01 <- rmapshaper::ms_simplify(SPDF, 0.01)
      SPDF_01@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
      datasetName_01 <- paste0(datasetName, "_01")
      message("Saving 1% version...\n")
      assign(datasetName_01, SPDF_01)
      save(list = datasetName_01, file = paste0(dataDir,"/",datasetName_01, '.RData'))
      rm(list = c("SPDF_01",datasetName_01))
    }

    # ----- Clean up and return --------------------------------------------------

    unlink(filePath, force = TRUE)
    unlink(dsnPath, recursive = TRUE, force = TRUE)

    return(invisible(datasetName))

}

