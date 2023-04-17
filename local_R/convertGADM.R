#' @keywords datagen
#' @importFrom rlang .data
#' @importFrom cleangeo clgeo_IsValid
#' @export
#'
#' @title Convert Global Administrative Areas (GADM) geopackage layer
#'
#' @param countryCode ISO-3166-1 alpha-2 country code.
#' @param admLevel Administrative level to be downloaded.
#' @param baseUrl Base URL for data queries.
#'
#' @description Create a simple features data frame for Global Administrative Areas.
#'
#' @details A layer from a Global Administrative Areas Geopackage is converted to a
#' simple features data frame with additional columns of data. The resulting file
#' will be created in the spatial data directory which is set with
#' \code{setSpatialDataDir()}.
#'
#' Version 4.1 of GADM was released on 16 July 2022
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
#' @return Name of the datasetName being created.
#' @references \url{https://gadm.org/data.html}
#' @references \url{https://gadm.org/metadata.html}

convertGADM <- function(
    countryCode = NULL,
    admLevel = 0,
    baseUrl = "https://geodata.ucdavis.edu/gadm/gadm4.1/gpkg"
) {

  # ----- Setup ----------------------------------------------------------------

  # Use package internal data directory
  dataDir <- getSpatialDataDir()

  # Specify the name of the dataset and file being created
  datasetName <- paste0('GADM_', countryCode, '_', admLevel)

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

  # NOTE:  We will use sf::st_read() below to read in a .gpkg file directly from
  # NOTE:  a URL rather than downloading it first.

  # NOTE:  The Contents of convertLayer() are copied in verbatim below because
  # NOTE:  that function checks for the presence of downloaded data which we
  # NOTE:  don't have in this case.

  # ----- Convert to SFDF ------------------------------------------------------

  # Example:
  #   https://geodata.ucdavis.edu/gadm/gadm4.1/gpkg/gadm41_GIN.gpkg

  # > sf::st_layers(url)
  # Driver: GPKG
  # Available layers:
  #   layer_name geometry_type features fields crs_name
  # 1  ADM_ADM_0 Multi Polygon        1      2   WGS 84
  # 2  ADM_ADM_1 Multi Polygon        8     11   WGS 84
  # 3  ADM_ADM_2 Multi Polygon       34     13   WGS 84
  # 4  ADM_ADM_3 Multi Polygon      336     16   WGS 84


  # Build appropriate request URL
  dsn <- paste0(baseUrl, '/gadm41_', ISO3, '.gpkg')
  layer <- paste0("ADM_ADM_", admLevel)

  SFDF <-

    # Read in data on its native projection
    sf::st_read(
      dsn,
      layer,
      ###...,
      query = NA,
      options = NULL,
      quiet = FALSE,
      geometry_column = 1L,
      type = 0,
      promote_to_multi = TRUE,
      stringsAsFactors = FALSE,                     # ensure FALSE
      int64_as_string = FALSE,
      check_ring_dir = TRUE,                        # change from default
      fid_column_name = character(0),
      drivers = character(0),
      wkt_filter = character(0),
      optional = FALSE
    ) %>%

    # Reproject to North America projection: https://epsg.io/4269
    sf::st_transform(sf::st_crs(4269))

  # ----- Select useful columns and rename -------------------------------------

  # NOTE:  Using dplyr recipe style gets a little awkward so we resort to base
  # NOTE:  in the dataframe manipulations below.

  if ( admLevel == 0 ) {

    # * Country level ----------------------------------------------------------

    # > dplyr::glimpse(SFDF, width = 75)
    # Rows: 1
    # Columns: 3
    # $ GID_0   <chr> "GIN"
    # $ COUNTRY <chr> "Guinea"
    # $ geom    <MULTIPOLYGON [°]> MULTIPOLYGON (((-8.707271 7...


    # Create the new dataframe in a specific column order
    SFDF <-
      SFDF %>%
      dplyr::rename(
        geometry = "geom"
      ) %>%
      dplyr::mutate(
        countryCode = iso3ToIso2(.data$GID_0),
        countryName = .data$COUNTRY
      ) %>%
      dplyr::mutate(
        HASC = .data$countryCode
      ) %>%
      dplyr::select(
        countryCode,
        countryName,
        HASC
      )

  } else if ( admLevel == 1 ) {

    # * State level ------------------------------------------------------------

    # > dplyr::glimpse(SFDF, width = 75)
    # Rows: 8
    # Columns: 12
    # $ GID_1     <chr> "GIN.1_1", "GIN.2_1", "GIN.3_1", "GIN.4_1", "GIN.5_1", …
    # $ GID_0     <chr> "GIN", "GIN", "GIN", "GIN", "GIN", "GIN", "GIN", "GIN"
    # $ COUNTRY   <chr> "Guinea", "Guinea", "Guinea", "Guinea", "Guinea", "Guin…
    # $ NAME_1    <chr> "Boké", "Conakry", "Faranah", "Kankan", "Kindia", "Labé…
    # $ VARNAME_1 <chr> "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA"
    # $ NL_NAME_1 <chr> "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA"
    # $ TYPE_1    <chr> "Region", "Region", "Region", "Region", "Region", "Regi…
    # $ ENGTYPE_1 <chr> "Region", "Region", "Region", "Region", "Region", "Regi…
    # $ CC_1      <chr> "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA"
    # $ HASC_1    <chr> "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA"
    # $ ISO_1     <chr> "NA", "GN-C", "NA", "GN-K", "NA", "NA", "NA", "NA"
    # $ geom      <MULTIPOLYGON [°]> MULTIPOLYGON (((-14.62264 1..., MULTIPOLYGON (((-13.758…

    # Data Dictionary:
    #   GID_1 --------> uniqueID
    #   GID_0 --------> ISO3
    #   COUNTRY ------> countryName: English language name
    #   NAME_1 -------> stateName: English language name
    #   VARNAME_1 ----> (drop)
    #   NL_NAME_1 ----> (drop)
    #   TYPE_1 -------> (drop)
    #   ENGTYPE_1 ----> (drop)
    #   CC_1 ---------> (drop)
    #   HASC_1 -------> HASC_1
    #   ISO_1 -------> HASC_1

    keepNames <- c("GID_0", "NAME_0", "NAME_1", "HASC_1")

    # NOTE:  Using dplyr recipe style gets a little awkward so we resort to base
    data <- SFDF[, keepNames]

    names(data) <- c("ISO3", "countryName", "stateName", "HASC")
    data$countryCode <- iso3ToIso2(data$ISO3)
    data$stateCode <- stringr::str_split_fixed(data$HASC, '\\.', 8)[ , 2]
    data <- data[, c("countryCode", "countryName", "stateCode", "stateName", "HASC")]

    SFDF <- data

  } else if ( admLevel == 2 ) {

    # * County level -----------------------------------------------------------

    # > dplyr::glimpse(SFDF, width = 80)
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
    data <- SFDF[, keepNames]

    names(data) <- c("ISO3", "countryName", "stateName", "countyName", "HASC")
    data$countryCode <- iso3ToIso2(data$ISO3)
    data$stateCode <- stringr::str_split_fixed(data$HASC, '\\.', n = 8)[ , 2]
    data <- data[, c("countryCode", "countryName", "stateCode", "stateName", "countyName", "HASC")]

    SFDF <- data

  } else {

    # * Lower level ------------------------------------------------------------

    # > dplyr::glimpse(SFDF, width = 80)
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
    data <- SFDF[, keepNames]

    names(data) <- c("ISO3", "countryName", "stateName", "countyName", extraNames)
    data$countryCode <- iso3ToIso2(data$ISO3)
    data <- data[, c("countryCode", "countryName", "stateName", "countyName", extraNames)]

    SFDF <- data

  }

  # ----- Simplify and save ----------------------------------------------------

  uniqueIdentifier <- "HASC"

  simplifyAndSave(
    SFDF = SFDF,
    datasetName = datasetName,
    uniqueIdentifier = uniqueIdentifier,
    dataDir = dataDir
  )

  # ----- Clean up and return --------------------------------------------------

  # # Clean up # NOTE:  No files were downloaded
  # unlink(filePath, force = TRUE)
  # unlink(dsnPath, recursive = TRUE, force = TRUE)

  return(invisible(datasetName))

}
