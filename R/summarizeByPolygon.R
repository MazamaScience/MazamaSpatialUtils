#' @keywords internal
#' @export
#' @title Summarize values by polygon
#' @param longitude vector of longitudes
#' @param latitude vector of latitudes
#' @param value vector of values at the locations of interest
#' @param SPDF SpatialPolygonsDataFrame with polygons used for aggregating
#' @param polygonName name of the SPDF column with the polygon names of interest
#' @param FUN function to be applied while summarizing (e.g. mean, max, etc.)
#' @param useBuffering passed to MazamaSpatialUtils::getSpatialData()
#' @description Given vectors of longitudes, latitudes and values, this function will summarize given values by
#' spatial polygon using the \code{FUN} and return a dataframe with polygon names and summary values.
#' @return a dataframe with two columns: polygonName, summary value

summarizeByPolygon <- function(longitude, latitude, value,
                             SPDF, polygonName, FUN, useBuffering=FALSE) {

  # Check the vectors of longitude, latitude and value have the same length
  if(any(length(longitude) != length(latitude), length(longitude) != length(value),
          length(latitude) != length(value)) ) {
    stop("longitude, latitude and value should have the same length")
  }
  if(!polygonName %in% names(SPDF)) {
       stop("polygonName not present in SPDF")
  }

  # Create df with longitude, latitude, value
  df <- data.frame( longitude = longitude,
                    latitude = latitude,
                    value = value)

  df$location <- paste0(longitude,'_',latitude)

  # To speed things up
  df_unique <- df[!duplicated(df$location),]

  df_unique$polygonName <- getSpatialData( lon=df_unique$longitude, lat=df_unique$latitude,
                                      SPDF=SPDF, useBuffering=useBuffering)[[polygonName]]

  rownames(df_unique) <- df_unique$location

  # Build back the full dataframe
  df$polygonName <- df_unique[df$location, 'polygonName']

  df <- dplyr::group_by(df, polygonName)
  df <- dplyr::summarise(df, FUN(value))
  df <- as.data.frame(df)
  names(df)[2] <- 'summaryValue'

  return(df)
}
