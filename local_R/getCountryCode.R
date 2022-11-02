#' @keywords locator
#' @export
#' @title Return country ISO codes at specified locations
#'
#' @param longitude Vector of longitudes in decimal degrees East.
#' @param latitude Vector of latitudes in decimal degrees North.
#' @param dataset Name of spatial dataset to use.
#' @param countryCodes Vector of ISO 3166-1 alpha-2 country codes.
#' @param allData Logical specifying whether a full dataframe should be returned.
#' @param useBuffering Logical flag specifying the use of location buffering to
#' find the nearest polygon if no target polygon is found.
#'
#' @description Uses spatial comparison to determine which country polygons the
#' locations fall into and returns the country code strings for those polygons.
#'
#' If \code{allData = TRUE}, additional data is returned.
#'
#' @return Vector of ISO-3166-1 alpha-2 country codes.
#'
#' @examples
#' library(MazamaSpatialUtils)
#'
#' longitude <- seq(0, 50)
#' latitude <- seq(0, 50)
#'
#' getCountryCode(longitude, latitude)
#'
#' @references http://www.naturalearthdata.com/downloads/10m-cultural-vectors/
#' @seealso SimpleCountries
#' @seealso getSpatialData
#'
getCountryCode <- function(
  longitude = NULL,
  latitude = NULL,
  dataset = "SimpleCountriesEEZ",
  countryCodes = NULL,
  allData = FALSE,
  useBuffering = FALSE
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(longitude)
  MazamaCoreUtils::stopIfNull(latitude)
  MazamaCoreUtils::stopIfNull(dataset)
  MazamaCoreUtils::stopIfNull(allData)
  MazamaCoreUtils::stopIfNull(useBuffering)

  # Check existence of dataset
  if ( !exists(dataset) ) {
    stop("Missing dataset. Please loadSpatialData(\"", dataset, "\")",
         call. = FALSE)
  }

  # Check longitude, latitude ranges
  if ( min(longitude, na.rm = TRUE) < -180 ||
       max(longitude, na.rm = TRUE) > 180) {
    stop("'longitude' must be specified in the range -180:180.")
  }
  if ( min(latitude, na.rm = TRUE) < -90 ||
       max(latitude, na.rm = TRUE) > 90 ) {
    stop("'latitude' must be specified in the range -90:90.")
  }

  # ----- Get the data ---------------------------------------------------------

  SPDF <- get(dataset)

  # Subset by country before searching
  if (!is.null(countryCodes))
    SPDF <- SPDF[SPDF$countryCode %in% countryCodes,]

  locationsDF <- getSpatialData(longitude, latitude, SPDF, useBuffering = useBuffering)

  if (allData) {

    return(locationsDF)

  } else {

    countryCode <- locationsDF$countryCode

    return(countryCode)

  }

}

