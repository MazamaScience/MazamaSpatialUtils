#' @keywords datagen
#' @importFrom rlang .data
#' @export
#'
#' @title Convert USGS hydrologic unit geodatabase
#'
#' @param gdbDir Directory containing the geodatabase.
#' @param level Character or integer which must be 2, 4, 6, 8, 10, 12 or 14.
#' @param simplify Logical specifying whether to perform simplification
#'
#' @description Create a simple features data frame for USGS watershed boundaries
#'
#' @details A USGS Watershed Boundary Dataset geodatabase is converted to a
#' simple features data frame with additional columns of data. To use this
#' function, the WBD geodatabase must be downloaded into a directory which is
#' identified with \code{gdbDir}. The resulting file will be created in the
#' spatial data directory which is set with \code{setSpatialDataDir()}.
#'
#' The full WBD datasetName can be downloaded from the USGS with the
#' following command:
#' \preformatted{
#' curl https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/WBD/National/GDB/WBD_National_GDB.zip -O
#' }
#'
#' The source data was downloaded on 2023-03-23.
#'
#' @note From the source documentation:
#'
#' The Watershed Boundary Dataset (WBD) is a comprehensive aggregated collection
#' of hydrologic unit data consistent with the national criteria for delineation
#' and resolution. It defines the areal extent of surface water drainage to a
#' point except in coastal or lake front areas where there could be multiple
#' outlets as stated by the &quot;Federal Standards and Procedures for the
#' National Watershed Boundary Dataset (WBD)&quot; &quot;Standard&quot;
#' (https://pubs.usgs.gov/tm/11/a3/). Watershed boundaries are determined solely
#' upon science-based hydrologic principles, not favoring any administrative
#' boundaries or special projects, nor particular program or agency. This dataset
#' represents the hydrologic unit boundaries to the 12-digit (6th level) for the
#' entire United States. Some areas may also include additional subdivisions
#' representing the 14- and 16-digit hydrologic unit (HU). At a minimum, the HUs
#' are delineated at 1:24,000-scale in the conterminous United States,
#' 1:25,000-scale in Hawaii, Pacific basin and the Caribbean, and 1:63,360-scale
#' in Alaska, meeting the National Map Accuracy Standards (NMAS). Higher
#' resolution boundaries are being developed where partners and data exist and
#' will be incorporated back into the WBD. WBD data are delivered as a dataset
#' of polygons and corresponding lines that define the boundary of the polygon.
#' WBD polygon attributes include hydrologic unit codes (HUC), size (in the form
#' of acres and square kilometers), name, downstream hydrologic unit code, type
#' of watershed, non-contributing areas, and flow modifications. The HUC
#' describes where the unit is in the country and the level of the unit. WBD
#' line attributes contain the highest level of hydrologic unit for each boundary,
#' line source information and flow modifications.
#'
#' The intent of defining Hydrologic Units (HU) within the Watershed Boundary
#' Dataset is to establish a base-line drainage boundary framework, accounting
#' for all land and surface areas. Hydrologic units are intended to be used as
#' a tool for water-resource management and planning activities particularly
#' for site-specific and localized studies requiring a level of detail provided
#' by large-scale map information. The WBD complements the National Hydrography
#' Dataset (NHD) and supports numerous programmatic missions and activities
#' including: watershed management, rehabilitation and enhancement, aquatic
#' species conservation strategies, flood plain management and flood prevention,
#' water-quality initiatives and programs, dam safety programs, fire assessment
#' and management, resource inventory and assessment, water data analysis and
#' water census.
#'
#' @return Name of the datasetName being created.
#'
#' @references \url{https://www.usgs.gov/national-hydrography/watershed-boundary-dataset}
#'
#' @seealso setSpatialDataDir

convertWBDHUC <- function(
  gdbDir = "~/Data/WBD/WBD_National_GDB.gdb",
  level = 2,
  simplify = TRUE
) {

  # ----- Setup ----------------------------------------------------------------

  # Use package internal data directory
  dataDir <- getSpatialDataDir()

  # 'level' should be a character string
  level <- as.character(level)

  # Specify the name of the dataset and file being created
  datasetName <- paste0('WBDHU', level)

  # Check existence of NaturalEarthAdm1 needed for assignment of stateCode
  if ( !exists("NaturalEarthAdm1") ) {
    stop("Missing datasetName. Please loadSpatialData(\"NaturalEarthAdm1\")",
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

  # ----- Convert to SFDF ------------------------------------------------------

  # > print(rgdal::ogrListLayers(gdbDir), width = 75)
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

  # Convert gdb layer into simple features data frame
  dsnPath <- gdbDir
  shpName <- datasetName
  SFDF <- convertLayer(
    dsn = dsnPath,
    layer = shpName
  )

  # ----- Select useful columns and rename -------------------------------------

  # For HUCs 2, 4:
  # > dplyr::glimpse(SFDF, width = 75)
  # Rows: 22
  # Columns: 16
  # $ tnmid             <chr> "{9F7DA17F-2AB2-4A27-B6AD-C3475BBA7C4A}", "{772…
  # $ metasourceid      <chr> "{593C2F8D-2FB2-441A-8138-E5D5D99739B3}", NA, "…
  # $ sourcedatadesc    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
  # $ sourceoriginator  <chr> "U.S. Geological Survey", NA, "U.S. Geological …
  # $ sourcefeatureid   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
  # $ loaddate          <dttm> 2022-02-22 06:15:19, 2019-12-05 03:32:03, 2022…
  # $ referencegnis_ids <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
  # $ areaacres         <dbl> 117262002, 3846772, 7159207, 2024969, 104270054…
  # $ areasqkm          <dbl> 474542.91, 15567.35, 28972.31, 8194.77, 421966.…
  # $ states            <chr> "LA,NM,TX", "PR", "HI", "AS,GU", "IL,IN,KY,MD,N…
  # $ huc2              <chr> "12", "21", "20", "22", "05", "06", "07", "08",…
  # $ name              <chr> "Texas-Gulf Region", "Caribbean Region", "Hawai…
  # $ globalid          <chr> "{35877C00-D26A-41C3-9F0D-206C9B5BE58E}", "{211…
  # $ shape_Length      <dbl> 43.12278, 10.10581, 21.38355, 14.80628, 61.4252…
  # $ shape_Area        <dbl> 44.8400439, 1.3295391, 2.5203778, 0.6881636, 43…
  # $ shape             <MULTIPOLYGON [°]> MULTIPOLYGON (((-103.5414 3..., MU…

  # NOTE:  Comments are relevant to the WBD as downloaded on 2023-03-23

  if ( level == '2' ) {
    # > pryr::object_size(SFDF)
    # 54.46 MB
    # > dim(SFDF)
    # [1] 22 16
    usefulColumns <- c('loaddate','areasqkm', 'states', 'huc2', 'name')
  } else if ( level == '4' ) {
    # > pryr::object_size(SFDF)
    # 179.78 MB
    # > dim(SFDF)
    # [1] 245  16
    usefulColumns <- c('loaddate','areasqkm', 'states', 'huc4', 'name')
  } else if ( level == '6' ) {
    # > pryr::object_size(SFDF)
    # 224.75 MB
    # > dim(SFDF)
    # [1] 405 16
    usefulColumns <- c('loaddate','areasqkm', 'states', 'huc6', 'name')
  } else if (level == '8' ) {
    # > pryr::object_size(SFDF)
    # 474.43 MB
    # > dim(SFDF)
    # [1] 2413 16
    usefulColumns <- c('loaddate','areasqkm', 'states', 'huc8', 'name')
  } else if (level == '10' ) {
    # > pryr::object_size(SFDF)
    # 1.21 GB
    # > dim(SFDF)
    # 18866 18
    # NOTE:  New "hutype" and "humod" fields
    # > print(names(SFDF), width = 75)
    # [1] "tnmid"             "metasourceid"      "sourcedatadesc"
    # [4] "sourceoriginator"  "sourcefeatureid"   "loaddate"
    # [7] "referencegnis_ids" "areaacres"         "areasqkm"
    # [10] "states"            "huc10"             "name"
    # [13] "hutype"            "humod"             "globalid"
    # [16] "shape_Length"      "shape_Area"        "shape"
    usefulColumns <- c('loaddate','areasqkm', 'states', 'huc10', 'name')
  } else if (level == '12' ) {
    # > pryr::object_size(SFDF)
    # ???
    # > dim(SFDF)
    # ???
    usefulColumns <- c('loaddate','areasqkm', 'states', 'huc12', 'name')
  } else if (level == '14' ) {
    # > pryr::object_size(SFDF)
    # ???
    # > dim(SFDF)
    # ???
    usefulColumns <- c('loaddate','areasqkm', 'states', 'huc14', 'name')
  }

  # NOTE:  Rename 'shape' to 'geometry' for consistency with other datasets
  # NOTE:  Rename 'huc#' to 'HUC' for consistency with other datasets
  SFDF <- SFDF[, usefulColumns]
  names(SFDF) <- c('loadDate', 'area', 'allStateCodes', 'HUC', 'HUCName', 'geometry')

  # Required by sf
  sf::st_geometry(SFDF) <- "geometry"

  # Change are from km^2 to m^2
  SFDF$area <- as.numeric(SFDF$area) * 1e6

  # ----- Remove South Pacific Region ------------------------------------------

  # NOTE:  This region causes problems with countryCode and stateCode and is not
  # NOTE:  useful for projects in North America.

  SFDF <-
    SFDF %>%
    dplyr::filter(!stringr::str_detect(.data$HUC, "^22"))

  # ----- Add countryCode and stateCode ----------------------------------------

  # TODO:  Larger HUCs are centered in the US, while at smaller levels entire
  # TODO:  HUCs may be in foreign countries (ie Canada). Find a way to eliminate
  # TODO:  smaller HUCs whose 'allStateCodes' is not a US State

  # NOTE:  We need to fix geometries before calculating centroids
  if ( any(!sf::st_is_valid(SFDF)) ) {
    message("Correcting invalid geometries...")
    SFDF <- SFDF %>% sf::st_make_valid()
  }

  # Calculate centroids to help add more metadata
  result <- try( {
    message("Calculating centroids...\n")
    # Ignore: "st_centroid assumes attributes are constant over geometries of x"
    suppressWarnings({
      centroids <- sf::st_centroid(SFDF)
    })
    lon <- sf::st_coordinates(centroids)[,1]
    lat <- sf::st_coordinates(centroids)[,2]
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
    lon <- rep(as.numeric(NA), nrow(SFDF))
    lat <- rep(as.numeric(NA), nrow(SFDF))
    for (i in seq_len(nrow(SFDF)) ) {
      bbox <- sf::st_bbox(SFDF$geometry[i])
      lon[i] <- mean(bbox[1,])
      lat[i] <- mean(bbox[2,])
    }
  }

  # Add longitude, latitude and countryCode
  SFDF$longitude <- lon
  SFDF$latitude <- lat

  message("Assigning countryCode...\n")
  suppressWarnings({
    SFDF$countryCode <-
      getCountryCode(
        lon,
        lat,
        countryCodes = c('US', 'CA', 'MX'),
        useBuffering = TRUE
      )
  })

  countryCodes <- unique(SFDF$countryCode)
  countryCodes <- countryCodes[!is.na(countryCodes)]

  # NOTE:  This can take quite a long time for HUC > 6.
  message("Assigning stateCode...\n")
  suppressWarnings({
    SFDF$stateCode <-
      getStateCode(
        lon,
        lat,
        countryCodes = countryCodes,
        useBuffering = TRUE
      )
  })

  # Hack to change missing stateCodes to the value from allStateCodes

  for ( i in seq_len(nrow(SFDF)) ) {
    if ( is.na(SFDF$stateCode[i]) ) {
      SFDF$stateCode[i] <- SFDF$allStateCodes[i]
    }
    # NOTE:  Still NA in the case of level 2 HUC named "United States Minor Outlying Islands"
    if ( !is.na(SFDF$stateCode[i]) ) {
      if ( stringr::str_length(SFDF$stateCode[i]) > 2 ) {
        SFDF$stateCode[i] <- substr(SFDF$stateCode[i], start = 1, stop = 2)
      }
    }
  }

  # Reorder
  preferredOrder <- c(
    "HUC",
    "HUCName",
    "countryCode",
    "stateCode",
    "allStateCodes",
    "longitude",
    "latitude",
    "area",
    "loadDate",
    "geometry"
  )

  SFDF <- SFDF %>% dplyr::select(dplyr::all_of(preferredOrder))

  # ----- Simplify and save ----------------------------------------------------

  uniqueIdentifier <- "HUC"

  if ( simplify ) {

  simplifyAndSave(
    SFDF = SFDF,
    datasetName = datasetName,
    uniqueIdentifier = uniqueIdentifier,
    dataDir = dataDir
  )

  } else {

    # NOTE:  Copied from simplifyAndSave

    message("\nFull resolution version...")
    if ( any(!sf::st_is_valid(SFDF)) ) {
      message("Correcting invalid geometries...")
      SFDF <- SFDF %>% sf::st_make_valid()
      ###SFDF <- SFDF %>% sf::st_make_valid() %>% sf::st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))
    }
    message("Saving full resolution version...")
    assign(datasetName, SFDF)
    save(list = c(datasetName), file = paste0(dataDir, '/', datasetName, '.rda'))
    rm(list = datasetName)

  }



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

  # PNW = HUC 17
  loadSpatialData("WBDHU2_01")
  plot(WBDHU2_01$geometry[15])

  loadSpatialData("WBDHU4_01")
  PNW <- WBDHU4_01 %>% dplyr::filter(stringr::str_detect(HUC, "^17"))
  plot(PNW$geometry)

  # Upper Columbia = HUC 1702
  loadSpatialData("WBDHU6_01")
  UC <- WBDHU6_01 %>% dplyr::filter(stringr::str_detect(HUC, "^1702"))
  plot(UC$geometry)





}
