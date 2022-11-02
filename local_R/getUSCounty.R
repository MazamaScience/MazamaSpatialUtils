#' @keywords locator
#' @export
#'
#' @title Return US county name at specified locations
#'
#' @param longitude Vector of longitudes in decimal degrees East.
#' @param latitude Vector of latitudes in decimal degrees North.
#' @param dataset Name of spatial dataset to use.
#' @param stateCodes Vector of US state codes used to limit the search.
#' @param allData Logical specifying whether a full dataframe should be returned.
#' @param useBuffering Logical flag specifying the use of location buffering to
#' find the nearest polygon if no target polygon is found.
#'
#' @description Uses spatial comparison to determine which county polygons the
#' locations fall into and returns the county name strings for those polygons.
#'
#' Specification of \code{stateCodes} limits spatial searching to the specified
#' states and greatly improves performance.
#'
#' If \code{allData = TRUE}, additional data is returned.
#'
#' @return Vector of English language county names.
#'
#' @examples
#' \dontrun{
#' library(MazamaSpatialUtils)
#' setSpatialDataDir("~/Data/Spatial")
#'
#' loadSpatialData("USCensusCounties")
#'
#' longitude <- seq(-140, -90)
#' latitude <- seq(20, 70)
#' getUSCounty(longitude, latitude)
#' }
#'
#' @references http://www.naturalearthdata.com/downloads/10m-cultural-vectors/
#' @seealso getSpatialData
#'
getUSCounty <- function(
  longitude = NULL,
  latitude = NULL,
  dataset = 'USCensusCounties',
  stateCodes = NULL,
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

  # Subset by state before searching
  if (!is.null(stateCodes)) SPDF <- SPDF[SPDF$stateCode %in% stateCodes,]

  locationsDF <- getSpatialData(longitude, latitude, SPDF, useBuffering = useBuffering)

  if (allData) {

    return(locationsDF)

  } else {

    name <- locationsDF$countyName

    return(name)

  }

}

