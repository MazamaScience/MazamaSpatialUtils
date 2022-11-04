#' @keywords locator
#' @export
#'
#' @title Return country names at specified locations
#'
#' @param longitude Vector of longitudes in decimal degrees East.
#' @param latitude Vector of latitudes in decimal degrees North.
#' @param datasetName Name of spatial dataset to use.
#' @param countryCodes Vector of ISO 3166-1 alpha-2 country codes.
#' @param allData Logical specifying whether a full dataframe should be returned.
#' @param useBuffering Logical flag specifying the use of location buffering to
#' find the nearest polygon if no target polygon is found.
#'
#' @description Uses spatial comparison to determine which country polygons the
#' locations fall into and returns the country name for those polygons.
#'
#' If \code{allData = TRUE}, additional data is returned.
#'
#' @return Vector of English language country names.
#'
#' @examples
#' library(MazamaSpatialUtils)
#'
#' longitude <- seq(0, 50)
#' latitude <- seq(0, 50)
#'
#' getCountryName(longitude, latitude)
#'
#' @seealso SimpleCountries
#' @seealso getSpatialData
getCountryName <- function(
  longitude = NULL,
  latitude = NULL,
  datasetName = "SimpleCountriesEEZ",
  countryCodes = NULL,
  allData = FALSE,
  useBuffering = FALSE
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(longitude)
  MazamaCoreUtils::stopIfNull(latitude)
  MazamaCoreUtils::stopIfNull(datasetName)
  MazamaCoreUtils::stopIfNull(allData)
  MazamaCoreUtils::stopIfNull(useBuffering)

  # Check existence of datasetName
  if ( !exists(datasetName) ) {
    stop("Missing datasetName. Please loadSpatialData(\"", datasetName, "\")",
         call. = FALSE)
  }

  MazamaCoreUtils::validateLonsLats(longitude, latitude, na.rm = TRUE)

  # ----- Get the data ---------------------------------------------------------

  SFDF <- get(datasetName)

  # Subset by country before searching
  if (!is.null(countryCodes))
    SFDF <- SFDF[SFDF$countryCode %in% countryCodes,]

  locationsDF <- getSpatialData(longitude, latitude, SFDF, useBuffering = useBuffering)

  if (allData) {

    return(locationsDF)

  } else {

    countryName <- locationsDF$countryName

    return(countryName)

  }

}

