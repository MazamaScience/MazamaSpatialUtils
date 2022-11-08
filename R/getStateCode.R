#' @keywords locator
#' @export
#' @title Return state ISO codes at specified locations
#'
#' @param longitude Vector of longitudes in decimal degrees East.
#' @param latitude Vector of latitudes in decimal degrees North.
#' @param datasetName Name of spatial dataset to use.
#' @param countryCodes Vector of ISO 3166-1 alpha-2 country codes.
#' @param allData Logical specifying whether a full dataframe should be returned.
#' @param useBuffering Logical flag specifying the use of location buffering to
#' find the nearest polygon if no target polygon is found.
#'
#' @description Uses spatial comparison to determine which 'state' polygons the
#' locations fall into and returns the ISO 3166 2-character state code
#' strings for those polygons.
#'
#' Specification of \code{countryCodes} limits spatial searching to the
#' specified countries and greatly improves performance.
#'
#' If \code{allData = TRUE}, additional data is returned.
#'
#' @return Vector of ISO-3166-2 alpha-2 state codes.
#'
#' @examples
#' \dontrun{
#' library(MazamaSpatialUtils)
#' setSpatialData("~/Data/Spatial_0.8")
#'
#' loadSpatialData("NaturalEarthAdm1")
#'
#' longitude <- seq(-140, -90)
#' latitude <- seq(20, 70)
#' getStateCode(longitude, latitude)
#' }
#' @seealso getSpatialData
getStateCode <- function(
  longitude = NULL,
  latitude = NULL,
  datasetName = "NaturalEarthAdm1",
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
    stop("Missing datasetName. Please loadSpatialData(\"",datasetName,"\")",
         call. = FALSE)
  }

  MazamaCoreUtils::validateLonsLats(longitude, latitude, na.rm = TRUE)

  # ----- Get the data ---------------------------------------------------------

  SFDF <- get(datasetName)

  # Subset by country before searching
  if ( !is.null(countryCodes) )
    SFDF <- SFDF[SFDF$countryCode %in% countryCodes,]

  locationsDF <- getSpatialData(longitude, latitude, SFDF, useBuffering = useBuffering)

  if (allData) {

    return(locationsDF)

  } else {

    stateCode <- locationsDF$stateCode

    return(stateCode)

  }

}

