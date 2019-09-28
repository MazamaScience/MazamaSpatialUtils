#' @keywords locator
#' @export
#' @title Get polygonID from SPDF of interest
#' @param SPDF spatial polygons dataset of interest
#' @description Extracts the the vector of unique polygon identifiers from 
#' \code{SPDF}.
#' 
#' This function is useful when writing code to aggregate data by polygon and
#' calculate per-polygon statistics. Each unique SpatialPolygonsDataFrame will 
#' have a different set of data columns but each is guaranteed to have a column 
#' named \code{polygonID} that uniquely identifies each polygon.
#' 
#' This allows us to write code that aggregates by polygon without having to 
#' know whether the polygons represent, countries, timezones or HUCs, etc.
#' @return Vector of polygon identifiers.
getPolygonID <- function(SPDF) {
  
  if ( !"SpatialPolygonsDataFrame" %in% class(SPDF) ) {
    
    stop(deparse(substitute(SPDF)), 
         ' is not a SpatialPolygonsDataFrame.', 
         call.=FALSE)
    
  } else {
    
    return(SPDF$polygonID)
    
  }
  
}

