#' @keywords internal
#' "_PACKAGE"
#' @name MazamaSpatialUtils
#' @title Mazama Science spatial data and utility functions.
#' @description This package contains code to convert various spatial datasets
#' into .rda files with uniformly named identifiers including:
#' \itemize{
#' \item{ countryCode -- ISO 3166-1 alpha-2}
#' \item{ countryName -- Country name}
#' \item{ stateCode -- ISO 3166-2 alpha-2}
#' \item{ timezone -- Olson timezone}
#' \item{ longitude -- degrees East}
#' \item{ latitude -- degrees North}
#' \item{ area -- m^2}
#' }
#' The only field guaranteed
#' to exist in every dataset is \code{countryCode}.
#'
#' The following additional standards are applied during the data conversion process:
#' \itemize{
#' \item{ all spatial data are converted to a purely geographic North American projection (\url{https://epsg.io/4269}) }
#' \item{ no duplicated rows in the dataframe (conversion to \strong{multi-}polygons) }
#' \item{ lowerCamelCase, human readable names replace original parameter names }
#' \item{ redundant, software-internal or otherwise unuseful data columns may be dropped }
#' \item{ latitude and longitude of polygon centroids may be added }
#' }
#'
#' Utility functions allow users to determine the country, state, county and timezones
#' associated with a set of locations, _e.g._ environmental monitoring sites.
#'
#' The uniformity of identifiers in the spatial datasets also makes it easy to generate maps
#' with data from any dataset that uses standard ISO codes for countries or states.
NULL


# ----- Internal Package State -------------------------------------------------

spatialEnv <- new.env(parent = emptyenv())
spatialEnv$dataDir <- NULL

#' @docType data
#' @name SpatialDataDir
#' @title Directory for spatial data
#' @format Absolute path string.
#' @description This package maintains an internal directory location which users can set
#' using \code{\link{setSpatialDataDir}}. All package functions use this directory whenever datasets
#' are created or loaded.
#'
#' The default setting when the package is loaded is \code{getwd()}.
#' @seealso getSpatialDataDir
#' @seealso setSpatialDataDir
NULL

#' @export
#' @title Get package data directory
#' @description Returns the package data directory where spatial data is located.
#' @return Absolute path string.
#' @seealso dataDir
#' @seealso setSpatialDataDir
getSpatialDataDir <- function() {
  if (is.null(spatialEnv$dataDir)) {
    stop('No data directory found. Please set a data directory with setSpatialDataDir("YOUR_DATA_DIR").',
         call. = FALSE)
  } else {
    return(spatialEnv$dataDir)
  }
}

#' @export
#' @title Set package data directory
#' @param dataDir Directory where spatial datasets are created.
#' @description Sets the package data directory where spatial data is located.
#' If the directory does not exist, it will be created.
#' @return Silently returns previous value of data directory.
#' @seealso SpatialDataDir
#' @seealso getSpatialDataDir
setSpatialDataDir <- function(dataDir) {
  old <- spatialEnv$dataDir
  dataDir <- path.expand(dataDir)
  tryCatch({
    if (!file.exists(dataDir)) dir.create(dataDir)
    spatialEnv$dataDir <- dataDir
  }, warning = function(warn) {
    warning("Invalid path name.")
  }, error   = function(err) {
    stop(paste0("Error in setSpatialDataDir(",dataDir,")."))
  })
  return(invisible(old))
}

#' @keywords internal
#' @export
#' @title Remove package data directory
#' @description Resets the package data dir to NULL. Used for internal testing.
#' @return Silently returns previous value of data directory.
#' @seealso SpatialDataDir
#' @seealso getSpatialDataDir
#' @seealso setSpatialDataDir
.removeSpatialDataDir <- function() {
  old <- spatialEnv$dataDir
  spatialEnv$dataDir <- NULL
  return(invisible(old))
}


# ----- Code <-> Name conversion functions  ------------------------------------

#' @export
#' @title Convert from ISO2 to ISO3 country codes
#' @param countryCodes Vector of ISO 3166-1 alpha-2 country codes.
#' @description Converts a vector of ISO 3166-1 alpha-2 codes to the
#' corresponding ISO 3166-1 alpha-3 codes.
#' @return A vector of ISO3 country codes
iso2ToIso3 <- function(countryCodes) {
  nonMissingCountryCodes <- countryCodes[!is.na(countryCodes)]
  if ( all(stringr::str_length(nonMissingCountryCodes) == 2) ) {
    # Create a vector of ISO3 identified by the countrycode package
    iso3Codes <- countrycode::countrycode(
      countryCodes, "iso2c", "iso3c",
      custom_match = c("AN" = "ANT")  # Custom match for Netherlands Antilles
    )
    return(iso3Codes)
  } else {
    stop('countryCodes must be all ISO 3166-1 alpha-2', call. = FALSE)
  }
}

#' @export
#' @title Convert from ISO3 to ISO2 country codes
#' @param countryCodes Vector of ISO 3166-1 alpha-3 codes.
#' @description Converts a vector of ISO 3166-1 alpha-3 codes to the
#' corresponding ISO 3166-1 alpha-2 codes.
#' @return A vector of ISO2 country codes
iso3ToIso2 <- function(countryCodes) {
  nonMissingCountryCodes <- countryCodes[!is.na(countryCodes)]
  if ( all(stringr::str_length(nonMissingCountryCodes) == 3) ) {
    # Create a vector of ISO2 identified by ISO3
    iso2Codes <- countrycode::countrycode(
      countryCodes, 'iso3c', 'iso2c',
      custom_match = c("ANT" = "AN")# custom match for Netherlands Antilles
    )
    return(iso2Codes)
  } else {
    stop('countryCodes must be all ISO 3166-1 alpha-3', call. = FALSE)
  }
}

#' @export
#' @title Convert country codes to country names
#' @param countryCodes Vector of ISO 3166-1 alpha-2 country codes.
#' @description Converts a vector of ISO 3166-1 alpha-2 codes to the
#' corresponding English names.
#'
#' @note This function is deprecated as of \strong{MazamaSpatialUtils 0.8.7}.
#' Please use \link{countryCodeToName} instead.
#'
#' @return A vector of English country names or NA.
codeToCountry <- function(countryCodes) {
  return(countryCodeToName(countryCodes));
}

#' @export
#' @title Convert country names to country codes
#' @param countryNames Vector of English language country names.
#' @description Converts a vector of English country names to the corresponding
#' ISO 3166-1 alpha-2 codes.
#'
#' @note This function is deprecated as of \strong{MazamaSpatialUtils 0.8.7}.
#' Please use \link{countryNameToCode} instead.
#'
#' @return A vector of ISO 3166-1 alpha-2 codes or NA.
countryToCode <- function(countryNames) {
  return(countryNameToCode(countryNames));
}

#' @export
#' @title Convert state codes to state nnames
#' @param stateCodes Vector of state codes.
#' @param countryCodes Vector of ISO-3166-1 alpha-2 country codes the state might be found in.
#' @param dataset Name of dataset containing state-level identifiers.
#' @description Converts a vector of ISO 3166-2 alpha-2 state codes to the
#' corresponding English names.
#' @details For this function to work, you must install and load the
#' "NaturalEarthAdm1" dataset.
#' @return A vector of English state names or NA.
#' @seealso convertNaturalEarthAdm1
codeToState <- function(
  stateCodes,
  countryCodes = NULL,
  dataset = "NaturalEarthAdm1"
) {

  # Sanity check
  if ( !exists(dataset) ) {
    stop('Missing database. Please loadSpatialData("', dataset, '")',
         call. = FALSE)
  }

  SFDF <- get(dataset)

  # Remove NA state codes
  stateTable <- SFDF[!is.na(SFDF$stateCode),]

  # Filter by countryCodes to make searching faster
  if (!is.null(countryCodes))
    stateTable <- stateTable[stateTable$countryCode %in% countryCodes,]

  # Test to see if any state codes have duplicate names
  for ( stateCode in stateCodes ) {

    repeatCount <- sum(stateTable$stateCode == stateCode)
    if ( repeatCount > 1 ) {
      warning(
        paste0(repeatCount, " states with code '", stateCode, "'. ",
               "Returning the first instance. ",
               "Please specify countryCode to return the state name from the desired country."))
    }
  }

  # Create a vector of state names identified by state code
  allStates <- stateTable$stateName
  names(allStates) <- stateTable$stateCode

  return(as.character(allStates[stateCodes]))

}

#' @export
#' @title Convert state names to state codes
#' @param stateNames Vector of state names to be converted.
#' @param countryCodes Vector of ISO 3166-2 alpha-2 country codes the state might be found in.
#' @param dataset Name of dataset containing state-level identifiers.
#' @description Converts a vector of state names to an ISO 3166-2 two character
#' state codes.
#' @details For this function to work, you must install and load the
#' "NaturalEarthAdm1" dataset.
#' @examples
#' \dontrun{
#' stateToCode("Washington")
#' stateToCode("Barcelona")
#' stateToCode("Shandong")
#' }
#' @return A vector of ISO 3166-2 codes or NA.
#' @seealso convertNaturalEarthAdm1
stateToCode <- function(
  stateNames,
  countryCodes = NULL,
  dataset = "NaturalEarthAdm1"
) {

  # Sanity check
  if ( !exists(dataset) ) {
    stop('Missing database. Please loadSpatialData("', dataset, '")',
         call. = FALSE)
  }

  SFDF <- get(dataset)

  # Remove NA state codes
  stateTable <- SFDF[!is.na(SFDF$stateCode),]

  # Filter by countryCodes to make searching faster
  if ( !is.null(countryCodes) )
    stateTable <- stateTable[stateTable$countryCode %in% countryCodes,]

  # Create a vector of state codes identified by name
  allCodes <- stateTable$stateCode
  names(allCodes) <- stateTable$stateName

  return(as.character(allCodes[stateNames]))

}


# ===== Mapshaper ==============================================================

#' @export
#' @title Simplify simple features data frame
#' @param SFDF Object of class simple features data frame.
#' @param keep Proportion of points to retain (0-1; default 0.05)
#' @param ... Arguments passed to \code{rmapshaper::ms_simplify()}
#' @description Simplify a simple features dataframe. This is a convenience
#' wrapper for \code{\link[rmapshaper]{ms_simplify}}
#' @return A simplified simple features dataframe.
#' @examples
#' \dontrun{
#' library(MazamaSpatialUtils)
#' FR <-
#'   SimpleCountries %>%
#'   dplyr::filter(countryCode == "FR")
#' par(mfrow = c(3, 3), mar = c(1, 1, 3, 1))
#' for (i in 9:1) {
#'   keep <- 0.1 * i
#'   geom <-
#'     FR %>%
#'     simplify(keep) %>%
#'     sf::st_geometry()
#'   plot(geom, main=paste0("keep = ", keep))
#' }
#' layout(1)
#' par(mar = c(5,4,4,2)+.1)
#' }

simplify <- function(
  SFDF,
  keep = 0.05,
  ...
) {
  SFDF_simple <- rmapshaper::ms_simplify(SFDF, keep, ...)
  return(SFDF_simple)
}


#' @export
#' @title Aggregate shapes in a simple features data frame
#' @param SFDF Object of class simple features data frame.
#' @param field Name of the field to dissolve on.
#' @param sum_fields Names of fields to sum.
#' @param copy_fields Names of fields to copy. The first instance of each field will be
#' copied to the aggregated feature
#' @param ... arguments passed to \code{rmapshaper::ms_dissolve()}
#' @description Aggregate shapes in a simple features dataframe. This is a
#' convenience wrapper for \code{\link[rmapshaper]{ms_dissolve}}.
#' @return A simple features dataframe with aggregated shapes.
#' @examples
#' \donttest{
#' regions <- dissolve(SimpleCountries, field = "UN_region", sum_fields = "area")
#' plot(regions)
#' dplyr::glimpse(regions)
#' }

dissolve <- function(
  SFDF,
  field = NULL,
  sum_fields = NULL,
  copy_fields = NULL,
  ...
) {

  if (!field %in% names(SFDF))
    stop(paste0("Field '", field, "' not found."))

  SFDF_dissolved <- rmapshaper::ms_dissolve(SFDF, field, sum_fields, copy_fields, ...)

  return(SFDF_dissolved)

}


