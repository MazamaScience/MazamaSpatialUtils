#' @keywords locator
#' @export
#'
#' @title Return HUCs at specified locations
#'
#' @param longitude Vector of longitudes in decimal degrees East.
#' @param latitude Vector of latitudes in decimal degrees North.
#' @param dataset Name of spatial dataset to use.
#' @param HUCs Vector of Hydrologic Unit Codes used to limit searches.
#' @param allData logical specifying whether a full dataframe should be returned
#' @param useBuffering Logical flag specifying the use of location buffering to
#' find the nearest polygon if no target polygon is found.
#'
#' @description Uses spatial comparison to determine which HUC polygons the
#' locations fall into and returns the HUC identifier strings for those polygons.
#'
#' Specification of \code{HUCs} limits spatial searching to the
#' specified HUCs and greatly improves performance. For instance, if searching
#' for level 10 HUCs in the Upper Columbia basin, it would make sense to first
#' search WBDHU4_01 to learn that the level 4 HUC is \code{1702}. Then you
#' can greatly improve search times for higher level HUCs by specifying:
#' \code{HUCs = c("1702")}.
#'
#' If \code{allData = TRUE}, additional data is returned.
#'
#' @return Vector of HUC identifiers.
#' @seealso getSpatialData

getHUC <- function(
  longitude = NULL,
  latitude = NULL,
  dataset = NULL,
  HUCs = NULL,
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

  # Use standard internal name (assumes pre-loaded dataset)
  SFDF <- get(dataset)

  # Identify HUC string partial matches to use as a mask
  if ( !is.null(HUCs) ) {
    HUCMask <- rep(FALSE, nrow(SFDF))
    for ( HUC in HUCs ) {
      regex <- paste0('^', HUC)
      mask <- stringr::str_detect(SFDF$HUC, regex)
      HUCMask <- HUCMask | mask
    }
    SFDF <- SFDF[HUCMask,]
  }

  # Pull out rows from SFDF based on point-in-polygon search
  locationsDF <- getSpatialData(longitude, latitude, SFDF, useBuffering = useBuffering)

  # ----- Return results ---------------------------------------------------------

  if ( allData ) {

    return(locationsDF)

  } else {

    HUC <- locationsDF$HUC
    HUCName <- locationsDF$HUCName

    return(HUC)
  }

}

