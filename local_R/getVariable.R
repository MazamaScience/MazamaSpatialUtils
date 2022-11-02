#' @keywords locator
#' @export
#'
#' @title Return SPDF variable at specified locations
#'
#' @param longitude Vector of longitudes in decimal degrees East.
#' @param latitude Vector of latitudes in decimal degrees North.
#' @param dataset Name of spatial dataset to use.
#' @param variable Name of dataframe column to be returned.
#' @param countryCodes Vector of ISO 3166-1 alpha-2 country codes.
#' @param allData Logical specifying whether a full dataframe should be returned.
#' @param useBuffering Logical flag specifying the use of location buffering to
#' find the nearest polygon if no target polygon is found.
#'
#' @description Uses spatial comparison to determine which polygons the
#' locations fall into and returns the variable associated with those polygons.
#'
#' If \code{allData = TRUE}, the entire dataframe is returned.
#'
#' @return Vector or dataframe.
#'
#' @examples
#' library(MazamaSpatialUtils)
#'
#' longitude <- seq(0, 50)
#' latitude <- seq(0, 50)
#'
#' getVariable(longitude, latitude, "SimpleCountries", "UN_region")
#'
#' @seealso getSpatialData
#'
getVariable <- function(
  longitude = NULL,
  latitude = NULL,
  dataset = NULL,
  variable = NULL,
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
  if ( is.null(dataset) || !exists(dataset) ) {
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

  # Check variable name
  if ( !is.null(variable) ) {
    if ( !(variable %in% names(SPDF)) ) {
      stop(paste0('Dataset ',
                  dataset,
                  ' does not contain the variable ',
                  variable),
           call. = FALSE)
    }
  }

  # Subset by country before searching
  if ( !is.null(countryCodes) )
    SPDF <- SPDF[SPDF$countryCode %in% countryCodes,]

  # Pull out rows from SPDF@data based on point-in-polygon search
  locationsDF <- getSpatialData(longitude, latitude, SPDF, useBuffering = useBuffering)

  # ----- Return results ---------------------------------------------------------

  if (allData) {

    return(locationsDF)

  } else {

    return(locationsDF[[variable]])

  }

}
