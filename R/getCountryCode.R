#' @keywords locator
#' @export
#' @title Return Country ISO Codes at Specified Locations
#' @param lon vector of longitudes in decimal degrees
#' @param lat vector of latitudes in decimal degrees
#' @param dataset name of spatial dataset to use
#' @param countryCodes vector of countryCodes
#' @param allData logical specifying whether a full dataframe should be returned
#' @param useBuffering logical flag specifying the use of location buffering to find the nearest polygon if no target polygon is found
#' @description Uses spatial comparison to determine which country polygons the 
#'     locations fall into and returns the country code strings for those polygons.
#'     
#'     If \code{allData=TRUE}, additional data is returned.
#' @return Vector of ISO-3166-1 alpha-2 country codes.
#' @examples
#' lon <- seq(0,50)
#' lat <- seq(0,50)
#' getCountryCode(lon,lat)
#' @references \url{http://www.naturalearthdata.com/downloads/10m-cultural-vectors/}
#' @seealso SimpleCountries
#' @seealso getSpatialData
getCountryCode <- function(lon, lat, dataset='SimpleCountriesEEZ', countryCodes=NULL, allData=FALSE, useBuffering=FALSE) {
  
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
  
  # Subset by country before searching
  if (!is.null(countryCodes)) SPDF <- SPDF[SPDF$countryCode %in% countryCodes,]
  
  locationsDF <- getSpatialData(lon, lat, SPDF, useBuffering=useBuffering)
  
  if (allData) {
    
    return(locationsDF)
    
  } else {
    
    countryCode <- locationsDF$countryCode
    
    return(countryCode)
    
  }
  
}

