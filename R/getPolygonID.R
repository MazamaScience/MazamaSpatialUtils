#' @keywords locator
#' @export
#' @title Get PolygonID from SPDF of interest
#' @param SPDF SpatialPolygonsDataFrame of interest
#' @description Uses spatial comparison to determine which timezone polygons the
#'     locations fall into and returns the Olson timezone strings for those polygons.
#' @return Vector of PolygonIDs
getPolygonID <- function(SPDF) {
    if(class(SPDF) != "SpatialPolygonsDataFrame") {
      stop(deparse(substitute(SPDF)), ' is not a SpatialPolygonsDataFrame.', call.=FALSE)
    } else {
    return(SPDF$polygonID)
    }
}
