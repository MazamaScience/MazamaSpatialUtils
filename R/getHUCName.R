#' @keywords locator
#' @export
#' @title Return HUC names at specified locations
#' @param lon vector of longitudes in decimal degrees
#' @param lat vector of latitudes in decimal degrees
#' @param dataset name of spatial dataset to use
#' @param HUCs vector of Hydrologic Unit Codes
#' @param allData logical specifying whether a full dataframe should be returned
#' @description Uses spatial comparison to determine which HUC polygons the 
#' locations fall into and returns the HUC names for those polygons.
#'     
#' If \code{allData = TRUE}, additional data is returned.
#' @return Vector of HUC names. 
#' @seealso getSpatialData

getHUCName <- function(
  lon, 
  lat, 
  dataset = "WBDHU10_02", 
  HUCs = NULL, 
  allData = FALSE
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
  locationsDF <- getSpatialData(lon, lat, SPDF)
  
  if (allData) {
    
    return(locationsDF)
    
  } else {
    
    HUC <- locationsDF$HUC
    HUCName <- locationsDF$HUCName
    
    return(HUCName)
    
  }
  
}

