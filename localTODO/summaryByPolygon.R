#' @keywords internal
#' @export
#' @param longitude vector of longitudes
#' @param latitude vector of latitudes
#' @param value vector of values at the locations
#' @param SPDF SpatialPolygonsDataFrame with polygons used for aggregating
#' @param polyID name of the SPDF column with the polygon ID (e.g. "HUC" for WBD datasets, "code_has" for NaturalEarthAdm1, etc.)
#' @param FUN function to be applied while summarizing (e.g. mean, max, etc.)
#' @param useBuffering passed to MazamaSpatialUtils::getSpatialData()
#' @description Given vectors of longitudes, latitudes and values, this function will summarize given values by 
#' spatial polygon using the \code{FUN} and return a dataframe with polygon ID's and summary values.
#' @return a dataframe with two columns: polyID, summary value

if (FALSE) {
  longitude <- c(120.383333, -110, 25.433333, 11.330556, 101.766667, -110, -110)
  latitude <- c(36.066667, 71, 36.416667, 43.318611, 36.633333, 71, 71)
  value <- c(80, 43, 29, 55, 12, 32, 23)
  SPDF <- NaturalEarthAdm1
  polyID <- 'code_hasc'
  FUN <- mean
  useBuffering <- FALSE
  
  summaryByPolygon(longitude, latitude, value, SPDF, polyID, FUN, useBuffering=FALSE)
}

summaryByPolygon <- function(longitude, latitude, value,
                             SPDF, polyID, FUN, useBuffering=FALSE) {
  
  # Check the vectors of longitude, latitude and value have the same length
  if( any(length(longitude) != length(latitude), length(longitude) != length(value), 
          length(latitude) != length(value)) ) {
    stop("longitude, latitude and value should have the same length")
  }
  
  # Create df with longitude, latitude, value
  df <- data.frame( longitude = longitude,
                    latitude = latitude,
                    value = value)
  
  df$location <- paste0(longitude,'_',latitude)
  
  # To speed things up
  df_unique <- df[!duplicated(df$location),]
  
  df_unique$polyID <- getSpatialData( lon=df_unique$longitude, lat=df_unique$latitude, 
                                      SPDF=SPDF, useBuffering=useBuffering)[[polyID]]
  
  rownames(df_unique) <- df_unique$location
  
  # Build back the full dataframe
  df$polyID <- df_unique[df$location, 'polyID']
  
  df <- dplyr::group_by(df, polyID)
  df <- dplyr::summarise(df, FUN(value))
  df <- as.data.frame(df)
  names(df)[2] <- 'summaryValue'
  
  return(df)
}