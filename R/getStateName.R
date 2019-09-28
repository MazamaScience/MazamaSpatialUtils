#' @keywords locator
#' @export
#' @title Return state names at specified locations
#' @param lon vector of longitudes in decimal degrees
#' @param lat vector of latitudes in decimal degrees
#' @param dataset name of spatial dataset to use
#' @param countryCodes vector of country codes
#' @param allData logical specifying whether a full dataframe should be returned
#' @param useBuffering logical flag specifying the use of location buffering to 
#' find the nearest polygon if no target polygon is found
#' @description Uses spatial comparison to determine which 'state' polygons the 
#' locations fall into and returns the ISO 3166-2 2-character state code
#' strings for those polygons.
#'     
#' Specification of \code{countryCodes} limits spatial searching to the 
#' specified countries and greatly improves performance.
#'     
#' If \code{allData = TRUE}, additional data is returned.
#' @return Vector of state names in English
#' @examples
#' \dontrun{
#' lon <- seq(-140,-90)
#' lat <- seq(20,70)
#' getStateName(lon,lat)
#' }
#' @seealso getSpatialData
getStateName <- function(
  lon, 
  lat, 
  dataset = "NaturalEarthAdm1", 
  countryCodes = NULL, 
  allData = FALSE, 
  useBuffering = FALSE
) {
  
  # ----- Validate parameters -------------------------------------------------- 
  
  # Check existence of dataset
  if ( !exists(dataset) ) {
    stop("Missing dataset. Please loadSpatialData(\"",dataset,"\")",
         call. = FALSE)
  }
  
  # Check lon, lat ranges
  if ( min(lon, na.rm = TRUE) < -180 || 
       max(lon, na.rm = TRUE) > 180) {
    stop("'lon' must be specified in the range -180:180.")
  }
  if ( min(lat, na.rm = TRUE) < -90 || 
       max(lat, na.rm = TRUE) > 90 ) {
    stop("'lat' must be specified in the range -90:90.")
  }
  
  # ----- Get the data ---------------------------------------------------------
  
  SPDF <- get(dataset)
  
  # Subset by country before searching
  if ( !is.null(countryCodes) ) 
    SPDF <- SPDF[SPDF$countryCode %in% countryCodes,]
  
  locationsDF <- getSpatialData(lon, lat, SPDF, useBuffering = useBuffering)
  
  if (allData) {
    
    return(locationsDF)
    
  } else {
    
    stateName <- locationsDF$stateName
    
    return(stateName)
    
  }
  
}

