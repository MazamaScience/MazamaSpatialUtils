#' @export
#' @importFrom rlang .data
#' @title Summarize values by polygon
#' @param longitude vector of longitudes
#' @param latitude vector of latitudes
#' @param value vector of values at the locations of interest
#' @param SPDF SpatialPolygonsDataFrame with polygons used for aggregating
#' @param useBuffering passed to MazamaSpatialUtils::getSpatialData()
#' @param FUN function to be applied while summarizing (e.g. mean, max, etc.)
#' @param varName variable name assigned to the summary variable
#' @description Given vectors of longitudes, latitudes and values, this function 
#' will summarize given values by spatial polygon using the \code{FUN} and return 
#' a dataframe with polygon IDs and summary values.
#' @note This function has not been thoroughly tested and is included
#' in the package for experimental use only.
#' @return A dataframe with the same rows as `SPDF@data` but containing only two 
#' columns: `polygonID` and the summary value.

summarizeByPolygon <- function(
  longitude, 
  latitude, 
  value,
  SPDF, 
  useBuffering = FALSE, 
  FUN, 
  varName = "summaryValue"
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  # Check the vectors of longitude, latitude and value have the same length
  if ( any(length(longitude) != length(latitude), 
           length(longitude) != length(value),
           length(latitude) != length(value)) ) {
    stop("longitude, latitude and value should have the same length")
  }
  
  if ( !"polygonID" %in% names(SPDF) ) {
    stop("polygonID not present in SPDF")
  }
  
  # ----- Summarize ------------------------------------------------------------
  
  # # Create df with longitude, latitude, value
  # df <- data.frame(longitude = longitude,
  #                  latitude = latitude,
  #                  value = value)
  # 
  # df$location <- paste0(longitude, '_', latitude)
  # 
  # # To speed things up, only work with unique locations
  # df_unique <- df[!duplicated(df$location),]
  # 
  # # Find polygonIDs associated with locations
  # df_unique$polygonID <- getSpatialData(lon = df_unique$longitude, 
  #                                       lat = df_unique$latitude,
  #                                       SPDF = SPDF, 
  #                                       useBuffering = useBuffering)$polygonID
  # 
  # # NOTE:  We drop locations that do not fall into any polygon
  # df_unique <- df_unique[!is.na(df_unique$polygonID),]
  # 
  # # Assign rownames
  # rownames(df_unique) <- df_unique$location
  # 
  # # Use rownames to pull 'polygonID' for all locations in the full df
  # df$polygonID <- df_unique[df$location, 'polygonID']
  # 
  # # NOTE:  We drop locations that do not fall into any polygon
  # df <- df[!is.na(df$polygonID),]
  # 
  # df <- dplyr::group_by(df, "polygonID") # see dplyr "Non-standard evaluation" vignette
  # df <- dplyr::summarise(df, FUN(value))
  # df <- as.data.frame(df)
  # names(df)[2] <- varName
  # rownames(df) <- df$polygonID
  
  #####
  #####
  #####

  # ----- Find unique locations and polygonIDs ---------------------------------
  
  # Create df with longitude, latitude, value
  df <- data.frame(longitude = longitude,
                   latitude = latitude,
                   value = value)
  
  df$location <- paste0(longitude, '_', latitude)
  
  # To speed things up, only work with distinct locations
  df_unique <-
    df %>%
    dplyr::distinct(.data$location, .keep_all = TRUE)
  
  # Find polygonIDs associated with locations
  df_unique$polygonID <- 
    getSpatialData(lon = df_unique$longitude, 
                   lat = df_unique$latitude,
                   SPDF = SPDF, 
                   useBuffering = useBuffering) %>%
    dplyr::pull(polygonID)
  
  # Drop locations that do not fall into any polygon
  df_unique <- df_unique[!is.na(df_unique$polygonID),]
  
  # Create a vector of polygonIDs named by location for easy lookup
  polygonID <- df_unique$polygonID
  names(polygonID) <- df_unique$location
  
  # ----- Calcuate summary values by polygonID ---------------------------------
  
  # Add polygonID to the full dataframe
  df$polygonID <- polygonID[df$location]
  
  # NOTE:  Drop locations that do not fall into any polygon
  df <- df[!is.na(df$polygonID),]
  
  # Calculate the summary values by polygonID
  summary <-
    df %>%
    dplyr::group_by(polygonID) %>%
    dplyr::summarise(DUMMY = FUN(value))

  names(df)[2] <- varName
  
  
  
  
  # Create a vector of summary values named by polygonID for easy lookup
  
  #####
  #####
  #####
  
  
  returnDF <- dplyr::left_join(SPDF@data, df, by = "polygonID")
  rownames(returnDF) <- returnDF$polygonID
  
  # ----- Return ---------------------------------------------------------------
  
  return(returnDF[,c("polygonID", varName)])
  
}
