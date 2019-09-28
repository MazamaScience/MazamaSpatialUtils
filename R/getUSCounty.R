#' @keywords locator
#' @export
#' @title Return US county name at specified locations
#' @param lon vector of longitudes in decimal degrees
#' @param lat vector of latitudes in decimal degrees
#' @param dataset name of spatial dataset to use
#' @param stateCodes vector of stateCodes used to limit the search
#' @param allData logical specifying whether a full dataframe should be returned
#' @param useBuffering logical flag specifying the use of location buffering to 
#' find the nearest polygon if no target polygon is found
#' @description Uses spatial comparison to determine which county polygons the 
#' locations fall into and returns the county name strings for those polygons.
#'     
#' Specification of \code{stateCodes} limits spatial searching to the specified 
#' states and greatly improves performance.
#'     
#' If \code{allData = TRUE}, additional data is returned.
#' @return Vector of county names in English.
#' @examples
#' \dontrun{
#' lon <- seq(-140, -90)
#' lat <- seq(20, 70)
#' getUSCounty(lon, lat)
#' }
#' @references \url{http://www.naturalearthdata.com/downloads/10m-cultural-vectors/}
#' @seealso getSpatialData
getUSCounty <- function(
  lon, 
  lat, 
  dataset = 'USCensusCounties', 
  stateCodes = NULL, 
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
  
  # Subset by state before searching
  if (!is.null(stateCodes)) SPDF <- SPDF[SPDF$stateCode %in% stateCodes,]
  
  locationsDF <- getSpatialData(lon, lat, SPDF, useBuffering = useBuffering)
  
  if (allData) {
    
    return(locationsDF)
    
  } else {
    
    name <- locationsDF$countyName
    
    return(name)
    
  }
  
}

