#' @keywords locator
#' @export
#' @title Return SPDF variable at specified locations
#' @param lon vector of longitudes in decimal degrees
#' @param lat vector of latitudes in decimal degrees
#' @param dataset name of spatial dataset to use
#' @param variable name of dataframe column to be returned
#' @param countryCodes vector of countryCodes
#' @param allData logical specifying whether a full dataframe should be returned
#' @description Uses spatial comparison to determine which polygons the 
#' locations fall into and returns the variable associated with those polygons.
#'     
#' If \code{allData = TRUE}, the entire dataframe is returned.
#' @return Vector or dataframe.
#' @examples
#' \dontrun{
#' loadSpatialData("NaturalEarthAdm1")
#' lon <- seq(0, 50)
#' lat <- seq(0, 50)
#' getVariable(lon, lat, "NaturalEarthAdm1", "gns_lang")
#' }
#' @seealso getSpatialData
getVariable <- function(
  lon, 
  lat, 
  dataset = NULL, 
  variable = NULL, 
  countryCodes = NULL, 
  allData = FALSE
) {
  
  # ----- Validate parameters -------------------------------------------------- 
  
  # Check existence of dataset
  if ( is.null(dataset) || !exists(dataset) ) {
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
  
  # Check variable name
  if ( !is.null(variable) ) {
    if ( !(variable %in% names(SPDF)) ) {
      stop(paste0('Dataset ',
                  dataset,
                  ' does not contain the variable ',
                  variable), 
           call.=FALSE)
    }
  }
  
  # Subset by country before searching
  if ( !is.null(countryCodes) ) 
    SPDF <- SPDF[SPDF$countryCode %in% countryCodes,]
  
  locationsDF <- getSpatialData(lon, lat, SPDF)
  
  if (allData) {
    
    return(locationsDF)
    
  } else {
    
    return(locationsDF[[variable]])
    
  }
  
}
