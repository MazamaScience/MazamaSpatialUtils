#' @keywords locator
#' @export
#' @title Return US County Name at Specified Locations
#' @param lon vector of longitudes in decimal degrees
#' @param lat vector of latitudes in decimal degrees
#' @param dataset name of spatial dataset to use
#' @param stateCodes vector of stateCodes used to limit the search
#' @param allData logical specifying whether a full dataframe should be returned
#' @description Uses spatial comparison to determine which county polygons the 
#'     locations fall into and returns the county name strings for those polygons.
#'     
#'     Specification of \code{stateCodes} limits spatial searching to the specified states
#'     and greatly improves performance.
#'     
#'     If \code{allData=TRUE}, additional data is returned.
#' @return Vector of county names in English.
#' @examples
#' \dontrun{
#' lon <- seq(-140,-90)
#' lat <- seq(20,70)
#' getUSCounty(lon,lat)
#' }
#' @references \url{http://www.naturalearthdata.com/downloads/10m-cultural-vectors/}
#' @seealso getSpatialData
getUSCounty <- function(lon, lat, dataset='USCensusCounties', stateCodes=NULL, allData=FALSE) {
  
  # Sanity check
  if (!exists(dataset)) {
    stop('Missing database. Please loadSpatialData("',dataset,'")',call.=FALSE)
  }
  # check if longitude and latitude falls in the right range
  if ( min(lon, na.rm=TRUE) < -180 || 
       max(lon, na.rm=TRUE) > 180 || 
       min(lat, na.rm=TRUE) < -90 || 
       max(lat, na.rm=TRUE) > 90 ) {
  stop('Longitude or latitude is not specified in the correct range. Please try again.')
  }

  SPDF <- get(dataset)
  
  # Subset by state before searching
  if (!is.null(stateCodes)) SPDF <- SPDF[SPDF$stateCode %in% stateCodes,]
  
  locationsDF <- getSpatialData(lon,lat,SPDF)
  
  if (allData) {
    
    return(locationsDF)
    
  } else {
    
    name <- locationsDF$countyName
    
    return(name)
    
  }
  
}

