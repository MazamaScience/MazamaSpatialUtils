#' @docType package
#' @name MazamaSpatialUtils
#' @title Mazama Science spatial data and utility functions.
#' @description This package contains code to convert various spatial datasets into .RData files 
#' with uniformly named identifiers including:
#' \itemize{
#' \item{ countryCode -- ISO 3166-1 alpha-2}
#' \item{ countryName -- Country name}
#' \item{ stateCode -- ISO 3166-2 alpha-2}
#' \item{ timezone -- Olson timezone}
#' \item{ longitude -- degrees East}
#' \item{ latitude -- degrees North}
#' \item{ area -- m^2}
#' }
#' The parameters listed above will be found in the @@data slot of each spatial 
#' dataset whose source data has an equivalent field. The only field guaranteed 
#' to exist in every dataset is \code{countryCode}.
#' 
#' The following additional standards are applied during the data conversion process:
#' \itemize{
#' \item{ all spatial data are converted to a purely geographic projection (\code{CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"}) }
#' \item{ no duplicated rows in the dataframe (conversion to \strong{multi-}polygons) }
#' \item{ lowerCamelCase, human readable names replace original parameter names }
#' \item{ redundant, software-internal or otherwise unuseful data columns may be dropped }
#' \item{ parameters may be added to the @@data dataframe }
#' \item{ latitude and longitude of polygon centroids may be added }
#' }
#' 
#' Utility functions allow users to determine the country, state, county and timezones
#' associated with a set of locations, e.g. environmental monitoring sites.
#' 
#' The uniformity of identifiers in the spatial datasets also makes it easy to generate maps
#' with data from any dataset that uses standard ISO codes for countries or states.
NULL

#' @docType data
#' @keywords datasets
#' @name SimpleCountriesEEZ
#' @title World country EEZ polygons 
#' @format A SpatialPolygonsDataFrame with 261 elements and 6 columns of data.
#' @description SimpleCountriesEEZ is a simplified world borders dataset with a  
#' 200 mile coastal buffer corresponding to Exclusive Economic Zones, suitable for 
#' quick spatial searches. This dataset is distributed with the package and is used by
#' default whenever a dataset with country polygons is required.
#' @details This dataset is equivalent to EEZCountries but with fewer columns of data.
#' @seealso convertEEZCountries
NULL

#' @docType data
#' @keywords datasets
#' @name SimpleCountries
#' @title World country polygons
#' @format A SpatialPolygonsDataFrame with 246 elements and 7 columns of data.
#' @description SimpleCountries is a simplified world borders dataset suitable for global maps
#' and quick spatial searches. This dataset is distributed with the package and is used by
#' default whenever a dataset with country polygons is required.
#' @details This dataset is equivalent to TMWorldBordersSimple but with fewer columns of data.
#' @seealso convertTMWorldBordersSimple
NULL


#' @docType data
#' @keywords datasets
#' @name SimpleTimezones
#' @title World timezone polygons
#' @format A SpatialPolygonsDataFrame with 1106 elements and 6 columns of data.
#' @description SimpleTimezones is a simplified world timezones dataset suitable for global maps
#' and quick spatial searches. This dataset is distributed with the package and is used by
#' default whenever a dataset with timezone polygons is required.
#' @details This dataset is a simplified version of WorldTimezones.
#' @seealso convertWorldTimezones
NULL


# ----- Internal Package State -------------------------------------------------

spatialEnv <- new.env(parent = emptyenv())
spatialEnv$dataDir <- NULL

#' @docType data
#' @keywords environment
#' @name SpatialDataDir
#' @title Directory for spatial data
#' @format Absolute path string.
#' @description This package maintains an internal directory location which users can set
#' using \code{setSpatialDataDir()}. All package functions use this directory whenever datasets
#' are created or loaded.
#' 
#' The default setting when the package is loaded is \code{getwd()}.
#' @seealso getSpatialDataDir
#' @seealso setSpatialDataDir
NULL

#' @keywords environment
#' @export
#' @import sp
#' @title Get package data directory
#' @description Returns the package data directory where spatial data is located.
#' @return Absolute path string.
#' @seealso dataDir
#' @seealso setSpatialDataDir
getSpatialDataDir <- function() {
  if (is.null(spatialEnv$dataDir)) {
    stop('No data directory found. Please set a data directory with setSpatialDataDir("YOUR_DATA_DIR").',call.=FALSE)
  } else {
    return(spatialEnv$dataDir)    
  }
}

#' @keywords environment
#' @export
#' @title Set package data directory
#' @param dataDir directory where spatial datasets are created
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

#' @keywords environment
#' @keywords internal
#' @export
#' @title Remove package data directory
#' @description Resets the package data dir to NULL. Used for internal testing. 
#' @return Silently returns previous value of data directory.
#' @seealso SpatialDataDir
#' @seealso getSpatialDataDir
#' @seealso setSpatialDataDir
removeSpatialDataDir <- function() {
  old <- spatialEnv$dataDir
  spatialEnv$dataDir <- NULL
}


# ----- Code <-> Name conversion functions  ------------------------------------

#' @keywords conversion
#' @export
#' @title Convert from ISO2 to ISO3 country codes
#' @param countryCodes vector of country codes to be converted
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
    stop('countryCodes must be all ISO 3166-1 alpha-2', call.=FALSE)
  }
}

#' @keywords conversion
#' @export
#' @title Convert from ISO3 to ISO2 country codes
#' @param countryCodes vector of country codes to be converted
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
    stop('countryCodes must be all ISO 3166-1 alpha-3', call.=FALSE)
  }
}

#' @keywords conversion
#' @export
#' @title Convert country codes to country names
#' @param countryCodes vector of country codes to be converted
#' @description Converts a vector of ISO 3166-1 alpha-2 codes to the 
#' corresponding English names.
#' @return A vector of English country names or NA.
codeToCountry <- function(countryCodes) {
  countryNames <- countrycode::countrycode(
    countryCodes, "iso2c", "country.name",
    custom_match = c("AN" = "Netherlands Antilles") # custom match for Netherlands Antilles
  )
  return(countryNames)
}

#' @keywords conversion
#' @export
#' @title Convert country names to country codes
#' @param countryNames vector of country names to be converted
#' @description Converts a vector of English country names to the corresponding 
#' ISO 3166-1 alpha-2 codes.
#' @return A vector of ISO 3166-1 alpha-2 codes or NA.
countryToCode <- function(countryNames) {
  countryCodes <- countrycode::countrycode(
    countryNames, "country.name", "iso2c",
    custom_match = c("Netherlands Antilles" = "AN") # custom match for Netherlands Antilles
  )
  return(countryCodes)
}

#' @keywords conversion
#' @export
#' @title Convert state codes to state nnames
#' @param stateCodes vector of state codes to be converted
#' @param countryCodes ISO-3166-1 alpha-2 country codes the state might be found in
#' @param dataset name of dataset containing state-level identifiers
#' @description Converts a vector of ISO 3166-2 alpha-2 state codes to the 
#' corresponding English names.
#' @details For this function to work, you must first run 
#' \code{initializeSpatialData()} to
#' download, convert and install the necessary spatial data.
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
  
  SPDF <- get(dataset)
  
  # Remove NA state codes
  stateTable <- SPDF@data[!is.na(SPDF@data$stateCode),]
  
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

#' @keywords conversion
#' @export
#' @title Convert state names to state codes
#' @param stateNames state names to be converted
#' @param countryCodes ISO 3166-2 alpha-2 country codes the state might be found in
#' @param dataset name of dataset containing state-level identifiers
#' @description Converts a vector of state names to an ISO 3166-2 two character 
#' state codes.
#' @details For this function to work, you must first run 
#' \code{initializeSpatialData()} to
#' download, convert and install the necessary spatial data.
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
         call.=FALSE)
  }
  
  SPDF <- get(dataset)
  
  # Remove NA state codes
  stateTable <- SPDF@data[!is.na(SPDF@data$stateCode),]
  
  # Filter by countryCodes to make searching faster
  if ( !is.null(countryCodes) ) 
    stateTable <- stateTable[stateTable$countryCode %in% countryCodes,]
  
  # Create a vector of state codes identified by name
  allCodes <- stateTable$stateCode
  names(allCodes) <- stateTable$stateName
  
  return(as.character(allCodes[stateNames]))
  
}

# ----- Simplification  --------------------------------------------------------

#' @export
#' @title Simplify SpatialPolygonsDataFrame
#' @param SPDF object of class SpatialPolygonsDataFrame
#' @param keep proportion of points to retain (0-1; default 0.05)
#' @param ... arguments passed to \code{rmapshaper::ms_simplify()}
#' @description Simplify a spatial polygons dataframe. This is a convenience
#' wrapper for \code{rmapshaper::ms_simplify()}
#' @return A simplified spatial polygons dataframe.
#' @examples 
#' \dontrun{
#' FR <- subset(SimpleCountries, countryCode == 'FR')
#' par(mfrow = c(3, 3), mar = c(1, 1, 3, 1))
#' for (i in 9:1) {
#'   keep <- 0.1 * i
#'   plot(simplify(FR, keep), main=paste0("keep = ", keep))
#' }
#' layout(1)
#' par(mar = c(5,4,4,2)+.1)
#' }

simplify <- function(
  SPDF, 
  keep = 0.05, 
  ...
) {
  SPDF_simple <- rmapshaper::ms_simplify(SPDF, keep, ...)
  return(SPDF_simple)
}


# ----- Dissolve --------------------------------------------------------------

#' @export
#' @title Aggregate shapes in a SpatialPolygonsDataFrame
#' @param SPDF object of class SpatialPolygonsDataFrame
#' @param field proportion of points to retain (0-1; default 0.05)
#' @param sum_fields fields to sum
#' @param copy_fields fields to copy. The first instance of each field will be 
#' copied to the aggregated feature
#' @param ... arguments passed to \code{rmapshaper::ms_dissolve()}
#' @description Aggregate shapes in a spatial polygons dataframe. This is a 
#' convenience wrapper for \code{rmapshaper::ms_dissolve()}
#' @return A spatial polygons dataframe with aggregated shapes.
#' @examples 
#' \donttest{
#' regions <- dissolve(SimpleCountries, field = "UN_region", sum_fields = "area")
#' plot(regions)
#' regions@data
#' }

dissolve <- function(
  SPDF, 
  field = NULL, 
  sum_fields = NULL, 
  copy_fields = NULL, 
  ...
) {
  
  if (!field %in% names(SPDF)) {
    stop(paste0("Field '", field, "' not found."))
  }
  SPDF_dissolved <- rmapshaper::ms_dissolve(SPDF, field, sum_fields, copy_fields, ...)
  
  return(SPDF_dissolved)
  
}


# ----- State codes -----------------------------------------------------------

#' @docType data
#' @keywords datasets
#' @name US_stateCodes
#' @title Dataframe of US state codes
#' @format A dataframe with 51 rows and 6 columns of data.
#' @description US_stateCodes contains the following columns of data for the
#' 50 United States plus the District of Columbia:
#' \itemize{
#' \item{\code{stateCode} -- e.g. MT}
#' \item{\code{stateName} -- e.g. Montana}
#' \item{\code{adm1_code} -- e.g. USA-3515}
#' \item{\code{code_hasc} -- e.g. US.MT}
#' \item{\code{fips} -- e.g. US30}
#' }
NULL


#' CONUS state codes
#'
#' @export
#' @docType data
#' @name CONUS
#' @title CONUS state codes
#' @format A vector with 49 elements
#' @description
#' State codes for the 48 contiguous states +DC that make up the CONtinental US.

CONUS <- c(     "AL","AZ","AR","CA","CO","CT","DE","FL","GA",
                "ID","IL","IN","IA","KS","KY","LA","ME","MD",
           "MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ",
           "NM","NY","NC","ND","OH","OK","OR","PA","RI","SC",
           "SD","TN","TX","UT","VT","VA","WA","WV","WI","WY",
           "DC"     )

#' US state codes
#'
#' @export
#' @docType data
#' @name US_52
#' @title US state codes
#' @format A vector with 52 elements
#' @description
#' State codes for the 50 states +DC +PR (Puerto Rico).

US_52 <- c("AK","AL","AZ","AR","CA","CO","CT","DE","FL","GA",
           "HI","ID","IL","IN","IA","KS","KY","LA","ME","MD",
           "MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ",
           "NM","NY","NC","ND","OH","OK","OR","PA","RI","SC",
           "SD","TN","TX","UT","VT","VA","WA","WV","WI","WY",
           "DC","PR")
