#' @export
#' @importFrom rlang .data
#'
#' @title Summarize values by polygon
#'
#' @param longitude vector of longitudes
#' @param latitude vector of latitudes
#' @param value vector of values at the locations of interest
#' @param SFDF simple features data frame with polygons used for aggregating
#' @param useBuffering passed to MazamaSpatialUtils::getSpatialData()
#' @param FUN function to be applied while summarizing (e.g. mean, max, etc.)
#' @param varName variable name assigned to the summary variable
#'
#' @description Given vectors of longitudes, latitudes and values, this function
#' will summarize given values by spatial polygon using the \code{FUN} and return
#' a dataframe with polygon IDs and summary values.
#'
#' @note This function has not been thoroughly tested and is included
#' in the package for experimental use only.
#'
#' @return A dataframe with the same rows as `SFDF` but containing only two
#' columns: `polygonID` and the summary value.

summarizeByPolygon <- function(
  longitude,
  latitude,
  value,
  SFDF,
  useBuffering = FALSE,
  FUN,
  varName = "summaryValue"
) {

  # ----- Validate parameters --------------------------------------------------

  # Check the vectors of longitude, latitude and value have the same length
  if ( any(length(longitude) != length(latitude),
           length(longitude) != length(value),
           length(latitude)  != length(value)) ) {
    stop("longitude, latitude and value should have the same length")
  }

  if ( !"polygonID" %in% names(SFDF) )
    stop("polygonID not present in SFDF")

  # ----- Get the data ---------------------------------------------------------

  # Create df with longitude, latitude, value
  df <- data.frame( longitude = longitude,
                    latitude = latitude,
                    value = value)

  df$location <- MazamaCoreUtils::createLocationID(longitude, latitude)

  # To speed things up, only work with unique locations
  df_unique <- df[!duplicated(df$location),]

  # Find polygonIDs associated with locations
  df_unique$polygonID <-
    getSpatialData(
      longitude = df_unique$longitude,
      latitude = df_unique$latitude,
      SFDF = SFDF,
      useBuffering = useBuffering
    ) %>%
    dplyr::pull(.data$polygonID)

  # NOTE:  We drop locations that do not fall into any polygon
  df_unique <- df_unique[!is.na(df_unique$polygonID),]

  # Assign rownames
  rownames(df_unique) <- df_unique$location

  # Use rownames to pull 'polygonID' for all locations in the full df
  df$polygonID <- df_unique[df$location, 'polygonID']

  # NOTE:  We drop locations that do not fall into any polygon
  df <- df[!is.na(df$polygonID),]

  # df <- dplyr::group_by_(df, "polygonID") # see dplyr "Non-standard evaluation" vignette
  # df <- dplyr::summarise(df, FUN(value))
  # df <- as.data.frame(df)
  # names(df)[2] <- varName
  # rownames(df) <- df$polygonID
  #
  # returnDF <- dplyr::left_join(SFDF, df, by="polygonID")
  # rownames(returnDF) <- returnDF$polygonID

  summary <-
    df %>%
    dplyr::group_by(.data$polygonID) %>%
    dplyr::summarise(DUMMY = FUN(value))

  DUMMY <- summary$DUMMY
  names(DUMMY) <- summary$polygonID

  returnDF <- SFDF
  returnDF[[varName]] <- DUMMY[SFDF$polygonID]

  # ----- Return results -------------------------------------------------------

  return(returnDF[,c("polygonID",varName)])

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {

  longitude = c(20.383333, -110, 25.433333, 11.330556, 101.766667, -110, -110)
  latitude = c(36.066667, 71, 36.416667, 43.318611, 36.633333, 71, 71)
  value = c(80, 43, 29, 55, 12, 32, 23)
  SFDF = SimpleCountries
  useBuffering = FALSE
  FUN = mean
  varName = "valueMean"

}
