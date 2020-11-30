#' @keywords locator
#' @export
#'
#' @title Return HUC names at specified locations
#'
#' @param longitude Vector of longitudes in decimal degrees East.
#' @param latitude Vector of latitudes in decimal degrees North.
#' @param dataset Name of spatial dataset to use.
#' @param HUCs Vector of Hydrologic Unit Codes.
#' @param allData Logical specifying whether a full dataframe should be returned
#' @param useBuffering Logical flag specifying the use of location buffering to
#' find the nearest polygon if not target polygon is found.
#'
#' @description Uses spatial comparison to determine which HUC polygons the
#' locations fall into and returns the HUC names for those polygons.
#'
#' If \code{allData = TRUE}, additional data is returned.
#' @return Vector of HUC names.
#' @seealso getSpatialData

getHUCName <- function(
  longitude,
  latitude,
  dataset = NULL,
  HUCs = NULL,
  allData = FALSE,
  useBuffering = FALSE
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(dataset)

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

  # Use standard internal name (assumes pre-loaded dataset)
  SPDF <- get(dataset)

  # Identify HUC string partial matches to use as a mask
  if ( !is.null(HUCs) ) {
    HUCMask <- rep(FALSE, nrow(SPDF))
    for ( HUC in HUCs ) {
      regex <- paste0('^', HUC)
      mask <- stringr::str_detect(SPDF@data$HUC, regex)
      HUCMask <- HUCMask | mask
    }
    SPDF <- SPDF[HUCMask,]
  }

  # Pull out rows from SPDF@data based on point-in-polygon search
  locationsDF <- getSpatialData(longitude, latitude, SPDF, useBuffering = useBuffering)

  # ----- Return results ---------------------------------------------------------

  if (allData) {

    return(locationsDF)

  } else {

    HUC <- locationsDF$HUC
    HUCName <- locationsDF$HUCName

    return(HUCName)

  }

}

