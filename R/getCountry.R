#' @keywords locator
#' @export
#' @title Return country names at specified locations
#' @param lon vector of longitudes in decimal degrees
#' @param lat vector of latitudes in decimal degrees
#' @param dataset name of spatial dataset to use
#' @param countryCodes vector of countryCodes
#' @param allData logical specifying whether a full dataframe should be returned
#' @param useBuffering logical flag specifying the use of location buffering to 
#' find the nearest polygon if no target polygon is found
#' @description Uses spatial comparison to determine which country polygons the 
#' locations fall into and returns the country name for those polygons.
#'     
#' If \code{allData = TRUE}, additional data is returned.
#' @return Vector of country names in English.
#' @examples
#' lon <- seq(0, 50)
#' lat <- seq(0, 50)
#' getCountry(lon, lat)
#' @references \url{http://www.naturalearthdata.com/downloads/10m-cultural-vectors/}
#' @seealso SimpleCountries
#' @seealso getSpatialData
getCountry <- function(
  lon, 
  lat, 
  dataset = "SimpleCountriesEEZ", 
  countryCodes = NULL, 
  allData = FALSE, 
  useBuffering = FALSE
) {
  
  # ----- Validate parameters -------------------------------------------------- 

  # Check existence of dataset
  if ( !exists(dataset) ) {
    stop("Missing dataset. Please loadSpatialData(\"", dataset, "\")",
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
  if (!is.null(countryCodes)) 
    SPDF <- SPDF[SPDF$countryCode %in% countryCodes,]
  
  locationsDF <- getSpatialData(lon, lat, SPDF, useBuffering = useBuffering)
  
  if (allData) {
    
    return(locationsDF)
    
  } else {
    
    countryName <- locationsDF$countryName
    
    return(countryName)
    
  }
  
  
}

