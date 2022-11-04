#' @keywords locator
#' @export
#'
#' @title Return Olson timezones at specified locations
#'
#' @param longitude Vector of longitudes in decimal degrees East.
#' @param latitude Vector of latitudes in decimal degrees North.
#' @param datasetName Name of spatial dataset to use.
#' @param countryCodes Vector of ISO 3166-1 alpha-2 country codes.
#' @param allData Logical specifying whether a full dataframe should be returned.
#' @param useBuffering Logical flag specifying the use of location buffering to
#' find the nearest polygon if no target polygon is found.
#'
#' @description Uses spatial comparison to determine which timezone polygons the
#' locations fall into and returns the Olson timezone strings for those polygons.
#'
#' Specification of \code{countryCodes} limits spatial searching to the
#' specified countries and greatly improves performance.
#'
#' If \code{allData=TRUE}, additional data is returned.
#'
#' @return Vector of Olson timezones.
#'
#' @examples
#' library(MazamaSpatialUtils)
#'
#' longitude <- seq(-120, -60, 5)
#' latitude <- seq(20, 80, 5)
#'
#' getTimezone(longitude, latitude)
#'
#' @references \url{https://github.com/evansiroky/timezone-boundary-builder}

#'
getTimezone <- function(
  longitude = NULL,
  latitude = NULL,
  datasetName = "SimpleTimezones",
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

  # Check existence of dataset
  if ( !exists(datasetName) ) {
    stop("Missing dataset. Please loadSpatialData(\"", datasetName, "\")",
         call. = FALSE)
  }

  MazamaCoreUtils::validateLonsLats(longitude, latitude, na.rm = TRUE)

  # ----- Get the data ---------------------------------------------------------

  SFDF <- get(datasetName)

  # Subset by country before searching
  if (!is.null(countryCodes))
    SFDF <- SFDF[SFDF$countryCode %in% countryCodes,]

  SFDF <- getSpatialData(longitude, latitude, SFDF, useBuffering = useBuffering)

  if (allData) {

    return(SFDF)

  } else {

    timezone <- SFDF$timezone

    return(timezone)

  }

}

