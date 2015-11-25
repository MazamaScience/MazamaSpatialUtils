#' @keywords locator
#' @export
#' @title Return State Names at Specified Locations
#' @param lon vector of longitudes in decimal degrees
#' @param lat vector of latitudes in decimal degrees
#' @param dataset name of spatial dataset to use
#' @param countryCodes vector of country codes
#' @param allData logical specifying whether to return a full dataframe
#' @description Uses spatial comparison to determine which 'state' polygons the 
#'     locations fall into and returns the ISO 3166-2 2-character state code
#'     strings for those polygons.
#'     
#'     Specification of \code{countryCodes} limits spatial searching to the specified
#'     countries and greatly improves performance.
#'     
#'     If \code{allData=TRUE}, additional data is returned.
#' @return Vector of state names in English.
#' @examples
#' \dontrun{
#' lon <- seq(-140,-90)
#' lat <- seq(20,70)
#' getState(lon,lat)
#' }
#' @seealso getSpatialData
getState <- function(lon, lat, dataset='WBDHU10-ms', stateCodes=NULL, allData=FALSE) {
  
  # Sanity check
  if (!exists(dataset)) {
    stop('Missing database. Please loadSpatialData("',dataset,'")',call.=FALSE)
  }
  
  SPDF <- get(paste0('~/Data/Spatial/WBD/', dataset))
  
  # Subset by state before searching
  #TODO: fix this so that it can account for any HUC that touches the state, such as 
  #ones that are in a the character string of multiple state codes. 
  
  if (!is.null(stateCodes)) SPDF <- SPDF[SPDF$stateCode %in% stateCodes | str_detect(SPDF$originalStateCode, stateCodes),]
  
  locationsDF <- getSpatialData(lon,lat,SPDF)
  
  if (allData) {
    
    return(locationsDF)
    
  } else {
    
    HUCCode <- locationsDF$HUC
    HUCName <- locationsDF$HUCName
    
    # Sanity check -- missing stateCode implies location over water  
    badMask <- is.na(HUCCode)
    if (sum(badMask) > 0) {
      if(is.null(stateCodes)) {
        warning(paste(sum(badMask),"locations appear to be over international waters and no state can be assigned"))
      } else {
        warning(paste(sum(badMask),"locations appear to be either over international waters or not in given countryCodes and no state can be assigned"))
      }
    }  
    
    return(stateName)
    
  }
  
  
}

