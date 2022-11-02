#' @keywords datagen
#' @importFrom rlang .data
#' @importFrom cleangeo clgeo_IsValid
#' @export
#'
#' @title Convert Global Administrative Areas (GADM) SPDF
#'
#' @param countryCode ISO-3166-1 alpha-2 country code
#' @param admLevel administrative level to be downloaded
#' @param nameOnly Logical specifying whether to only return the name without
#' creating the file.
#' @param baseUrl Base URL for data queries.
#'
#' @description Create a SpatialPolygonsDataFrame for Global Administrative Areas.
#'
#' @details A pre-generated Global Administrative Areas SpatialPolygonsDataFrame
#' is downloaded and amended with additional columns of data. The resulting file
#' will be created in the spatial data directory which is set with
#' \code{setSpatialDataDir()}.
#'
#' The \code{@data} slot of each SpatialPolygonsDataFrame is both simplified and
#' modified to adhere to the \pkg{MazamaSpatialUtils} internal standards.
#'
#' @note Unlike other \code{convert~()} functions, no checks, cleanup or
#' simplification is performed.
#'
#' @note From the source documentation:
#'
#' GADM has always had multiple unique IDs associated with a record. In the
#' future there will be only be the GID and associated tables that link the GID
#' to other ID systems such as ISO, FIPS, and HASC. The GID starts with the three
#' letter ISO 3166-1 alpha-3 country code. If there are subdivisions these are
#' identified by a number from 1 to n, where n is the number of subdivisions at
#' level 1. This value is concatenated with the country code, using a dot to
#' delimit the two. For example, AFG.1, AFG.2, ..., AFG.n. If there are second
#' level subdivisions, numeric codes are assigned within each first level
#' subdivision and these are concatenated with the first level identifier, using
#' a dot as delimiter. For example, AFG.1.1, AFG.1.2, AFG.1.3, ..., and AFG.2.1,
#' AFG.2.2, .... And so forth for the third, fourth and fifth levels. Finally
#' there is an underscore followed by a version number appended to the code. For
#' example, AFG.3_1 and AFG.3.2_1. The GID codes are presistent after version
#' 3.6 (there were errors in the codes in version 3.4). If an area changes, for
#' example if it splits into two new areas, two new codes will be assigned, and
#' the old code will not be used any more. The version only changes when there
#' is a major overhaul of the divisions in a country, for example when a whole
#' new set of subdivsions is introduced.
#'
#' @return Name of the dataset being created.
#' @references \url{https://gadm.org/data.html}
#' @references \url{https://gadm.org/metadata.html}

convertGADM <- function(
  countryCode = NULL,
  admLevel = 0,
  nameOnly = FALSE,
  baseUrl = "https://biogeo.ucdavis.edu/data/gadm3.6/Rsp"
) {

  # ----- Setup ----------------------------------------------------------------

  # Use package internal data directory
  dataDir <- getSpatialDataDir()

  # Specify the name of the dataset and file being created
  datasetName <- paste0('GADM_', countryCode, '_', admLevel)

  if (nameOnly)
    return(datasetName)

  # Convert 2-character codes into ISO3
  ISO3 <- iso2ToIso3(countryCode)
  if ( is.na(ISO3) ) {
    stop(
      paste0(
        'The countryCode parameter "',
        countryCode,
        '" is not an ISO-3166-1 alpha-2 country code.'
      ),
      call. = FALSE
    )
  }

  # ----- Get the data ---------------------------------------------------------

  # Example:
  #   https://biogeo.ucdavis.edu/data/gadm3.6/Rsp/gadm36_AGO_0_sp.rds

  # Build appropriate request URL
  url <- paste0(
    baseUrl, '/gadm36_',
    ISO3, '_', admLevel, '_sp.rds'
  )

  # Get the data
  tempfile <- base::tempfile("spatial_data", fileext = ".rds")
  utils::download.file(url, tempfile)

  # ----- Convert to SPDF ------------------------------------------------------

  # Convert shapefile into SpatialPolygonsDataFrame
  SPDF <- readRDS(tempfile)
  base::file.remove(tempfile)

  # ----- Select useful columns and rename -------------------------------------

  # NOTE:  Using dplyr recipe style gets a little awkward so we resort to base
  # NOTE:  in the dataframe manipulations below.

  if ( admLevel == 0 ) {

    # * Country level ----------------------------------------------------------

    # > dplyr::glimpse(SPDF@data, width = 80)
    # Rows: 1
    # Columns: 2
    # $ GID_0  <chr> "AGO"
    # $ NAME_0 <chr> "Angola"

    keepNames <- c("GID_0", "NAME_0")

    data <- SPDF@data[, keepNames]

    names(data) <- c("ISO3", "countryName")
    data$countryCode <- iso3ToIso2(data$ISO3)
    data$HASC <- data$countryCode
    data <- data[, c("countryCode", "countryName", "HASC")]

    SPDF@data <- data

    uniqueIdentifier <- "ISO3"

  } else if ( admLevel == 1) {

    # * State level ------------------------------------------------------------

    # > dplyr::glimpse(SPDF@data, width = 80)
    # Rows: 18
    # Columns: 10
    # $ GID_0     <chr> "AGO", "AGO", "AGO", "AGO", "AGO", "AGO", "AGO", "AGO", "AG…
    # $ NAME_0    <chr> "Angola", "Angola", "Angola", "Angola", "Angola", "Angola",…
    # $ GID_1     <chr> "AGO.1_1", "AGO.2_1", "AGO.3_1", "AGO.4_1", "AGO.5_1", "AGO…
    # $ NAME_1    <chr> "Bengo", "Benguela", "Bié", "Cabinda", "Cuando Cubango", "C…
    # $ VARNAME_1 <chr> NA, "Benguella", NA, NA, NA, "Cuanza-Nord|Kwanza Norte", "C…
    # $ NL_NAME_1 <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    # $ TYPE_1    <chr> "Província", "Província", "Província", "Província", "Provín…
    # $ ENGTYPE_1 <chr> "Province", "Province", "Province", "Province", "Province",…
    # $ CC_1      <chr> "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "1…
    # $ HASC_1    <chr> "AO.BO", "AO.BG", "AO.BI", "AO.CB", "AO.CC", "AO.CN", "AO.C…

    # Data Dictionary:
    #   GID_0 --------> ISO3
    #   NAME_0 -------> countryName: English language name
    #   GID_1 --------> (drop)
    #   NAME_1 -------> stateName: English language name
    #   VARNAME_1 ----> (drop)
    #   NL_NAME_1 ----> (drop)
    #   TYPE_1 -------> (drop)
    #   ENGTYPE_1 ----> (drop)
    #   CC_1 ---------> (drop)
    #   HASC_1 -------> HASC_1

    keepNames <- c("GID_0", "NAME_0", "NAME_1", "HASC_1")

    # NOTE:  Using dplyr recipe style gets a little awkward so we resort to base
    data <- SPDF@data[, keepNames]

    names(data) <- c("ISO3", "countryName", "stateName", "HASC")
    data$countryCode <- iso3ToIso2(data$ISO3)
    data$stateCode <- stringr::str_split_fixed(data$HASC, '\\.', 8)[ , 2]
    data <- data[, c("countryCode", "countryName", "stateCode", "stateName", "HASC")]

    SPDF@data <- data

  } else if ( admLevel == 2 ) {

    # * County level -----------------------------------------------------------

    # > dplyr::glimpse(SPDF@data, width = 80)
    # Rows: 163
    # Columns: 13
    # $ GID_0     <chr> "AGO", "AGO", "AGO", "AGO", "AGO", "AGO", "AGO", "AGO", "AG…
    # $ NAME_0    <chr> "Angola", "Angola", "Angola", "Angola", "Angola", "Angola",…
    # $ GID_1     <chr> "AGO.1_1", "AGO.1_1", "AGO.1_1", "AGO.1_1", "AGO.1_1", "AGO…
    # $ NAME_1    <chr> "Bengo", "Bengo", "Bengo", "Bengo", "Bengo", "Benguela", "B…
    # $ NL_NAME_1 <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    # $ GID_2     <chr> "AGO.1.1_1", "AGO.1.2_1", "AGO.1.3_1", "AGO.1.4_1", "AGO.1.…
    # $ NAME_2    <chr> "Ambriz", "Dande", "Icolo e Bengo", "Muxima", "Nambuangongo…
    # $ VARNAME_2 <chr> NA, NA, NA, "Kissama", NA, "Baia Farta", NA, NA, NA, "Kaimb…
    # $ NL_NAME_2 <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    # $ TYPE_2    <chr> "Município", "Município", "Município", "Município", "Municí…
    # $ ENGTYPE_2 <chr> "Municpality|City Council", "Municpality|City Council", "Mu…
    # $ CC_2      <chr> "0104", "0101", "0102", "0103", "0105", "0202", "0206", "02…
    # $ HASC_2    <chr> "AO.BO.AM", "AO.BO.DA", "AO.BO.IB", "AO.BO.MU", "AO.BO.NA",…

    keepNames <- c("GID_0", "NAME_0", "NAME_1", "NAME_2", "HASC_2")

    # NOTE:  Using dplyr recipe style gets a little awkward so we resort to base
    data <- SPDF@data[, keepNames]

    names(data) <- c("ISO3", "countryName", "stateName", "countyName", "HASC")
    data$countryCode <- iso3ToIso2(data$ISO3)
    data$stateCode <- stringr::str_split_fixed(data$HASC, '\\.', n = 8)[ , 2]
    data <- data[, c("countryCode", "countryName", "stateCode", "stateName", "countyName", "HASC")]

    SPDF@data <- data

  } else {

    # * Lower level ------------------------------------------------------------

    # > dplyr::glimpse(SPDF@data, width = 80)
    # Rows: 527
    # Columns: 16
    # $ GID_0     <chr> "AGO", "AGO", "AGO", "AGO", "AGO", "AGO", "AGO", "AGO", "AG…
    # $ NAME_0    <chr> "Angola", "Angola", "Angola", "Angola", "Angola", "Angola",…
    # $ GID_1     <chr> "AGO.1_1", "AGO.1_1", "AGO.1_1", "AGO.1_1", "AGO.1_1", "AGO…
    # $ NAME_1    <chr> "Bengo", "Bengo", "Bengo", "Bengo", "Bengo", "Bengo", "Beng…
    # $ NL_NAME_1 <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    # $ GID_2     <chr> "AGO.1.1_1", "AGO.1.1_1", "AGO.1.1_1", "AGO.1.2_1", "AGO.1.…
    # $ NAME_2    <chr> "Ambriz", "Ambriz", "Ambriz", "Dande", "Dande", "Dande", "D…
    # $ NL_NAME_2 <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    # $ GID_3     <chr> "AGO.1.1.1_1", "AGO.1.1.2_1", "AGO.1.1.3_1", "AGO.1.2.1_1",…
    # $ NAME_3    <chr> "Ambriz", "Bela Vista", "Tabi", "Barra do Dande", "Caxito",…
    # $ VARNAME_3 <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    # $ NL_NAME_3 <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    # $ TYPE_3    <chr> "Commune", "Commune", "Commune", "Commune", "Commune", "Com…
    # $ ENGTYPE_3 <chr> "Commune", "Commune", "Commune", "Commune", "Commune", "Com…
    # $ CC_3      <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    # $ HASC_3    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…

    # NOTE:  We have no HASC field to generate stateCodes with. Anyone down this
    # NOTE:  far will have to get creative.

    keepNames <- c("GID_0", paste0("NAME_", 0:admLevel))
    extraNames <- paste0("NAME_", 3:admLevel)

    # NOTE:  Using dplyr recipe style gets a little awkward so we resort to base
    data <- SPDF@data[, keepNames]

    names(data) <- c("ISO3", "countryName", "stateName", "countyName", extraNames)
    data$countryCode <- iso3ToIso2(data$ISO3)
    data <- data[, c("countryCode", "countryName", "stateName", "countyName", extraNames)]

    SPDF@data <- data

  }

  # ----- Clean SPDF -----------------------------------------------------------

  # # Group polygons with the same identifier (ISO3)
  # SPDF <- organizePolygons(
  #   SPDF,
  #   uniqueID = 'ISO3',
  #   sumColumns = NULL
  # )
  #
  # # Clean topology errors
  # if ( !cleangeo::clgeo_IsValid(SPDF) ) {
  #   SPDF <- cleangeo::clgeo_Clean(SPDF, verbose = TRUE)
  # }

  # ----- Name and save the data -----------------------------------------------

  # Assign a name and save the data
  message("Saving full resolution version...\n")
  assign(datasetName, SPDF)
  save(list = c(datasetName), file = paste0(dataDir, '/', datasetName, '.rda'))
  rm(list = datasetName)

  # ----- Simplify -------------------------------------------------------------

  # if ( simplify ) {
  #   # Create new, simplified datsets: one with 5%, 2%, and one with 1% of the vertices of the original
  #   # NOTE:  This may take several minutes.
  #   message("Simplifying to 5%...\n")
  #   SPDF_05 <- rmapshaper::ms_simplify(SPDF, 0.05)
  #   SPDF_05@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
  #   # Clean topology errors
  #   if ( !cleangeo::clgeo_IsValid(SPDF_05) ) {
  #     SPDF_05 <- cleangeo::clgeo_Clean(SPDF_05)
  #   }
  #   datasetName_05 <- paste0(datasetName, "_05")
  #   message("Saving 5% version...\n")
  #   assign(datasetName_05, SPDF_05)
  #   save(list = datasetName_05, file = paste0(dataDir,"/", datasetName_05, '.rda'))
  #   rm(list = c("SPDF_05",datasetName_05))
  #
  #   message("Simplifying to 2%...\n")
  #   SPDF_02 <- rmapshaper::ms_simplify(SPDF, 0.02)
  #   SPDF_02@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
  #   # Clean topology errors
  #   if ( !cleangeo::clgeo_IsValid(SPDF_02) ) {
  #     SPDF_02 <- cleangeo::clgeo_Clean(SPDF_02)
  #   }
  #   datasetName_02 <- paste0(datasetName, "_02")
  #   message("Saving 2% version...\n")
  #   assign(datasetName_02, SPDF_02)
  #   save(list = datasetName_02, file = paste0(dataDir,"/", datasetName_02, '.rda'))
  #   rm(list = c("SPDF_02",datasetName_02))
  #
  #   message("Simplifying to 1%...\n")
  #   SPDF_01 <- rmapshaper::ms_simplify(SPDF, 0.01)
  #   SPDF_01@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
  #   # Clean topology errors
  #   if ( !cleangeo::clgeo_IsValid(SPDF_01) ) {
  #     SPDF_01 <- cleangeo::clgeo_Clean(SPDF_01)
  #   }
  #   datasetName_01 <- paste0(datasetName, "_01")
  #   message("Saving 1% version...\n")
  #   assign(datasetName_01, SPDF_01)
  #   save(list = datasetName_01, file = paste0(dataDir,"/", datasetName_01, '.rda'))
  #   rm(list = c("SPDF_01",datasetName_01))
  # }

  # ----- Clean up and return --------------------------------------------------

  # Clean up
  # unlink(filePath, force = TRUE)
  # unlink(dsnPath, recursive = TRUE, force = TRUE)

  return(invisible(datasetName))

}
