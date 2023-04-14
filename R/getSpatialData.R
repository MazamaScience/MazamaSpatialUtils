#' @export
#' @importFrom sf st_as_sf st_set_crs st_intersects st_is_within_distance
#'
#' @title Return spatial data associated with a set of locations
#'
#' @param longitude Vector of longitudes in decimal degrees East.
#' @param latitude Vector of latitudes in decimal degrees North.
#' @param SFDF sf dataframe with MULTIPOLYGON geometry.
#' @param useBuffering Logical flag specifying the use of location buffering to
#' find the nearest polygon if no target polygon is found.
#' @param verbose Logical flag controlling detailed progress statements.
#'
#' @return Dataframe of data.
#'
#' @description All locations are first converted to \code{SpatialPoints}
#' objects. The \code{\link[sf]{st_intersects}} function is then used to determine which
#' polygon from \code{SFDF} each location falls in. The dataframe row associated
#' with each polygon is then associated with each location.
#'
#' @details For coastal locations, locations may lie just outside the boundaries
#' of an individual polygon, especially if it is of low resolution.
#' To account for this any location that remains unassociated after the first
#' pass is checked to seee if it is with a specific distance of any polygon.
#' The set of distances is gradually increased until a polygon is reached or the
#' maximum distances is encountered. Distances include: 1km, 2km, 5km, 10km,
#' 20km, 50km, 100km, 200km. If a location is more than 200km away from any
#' polygon, a data frame record with all \code{NA}s is returned for that
#' location.
#'
#' Missing or invalid values in the incoming \code{longitude} or \code{latitude}
#' vectors result in records with all \code{NA}s at those positions in the
#' returned data frame.

getSpatialData <- function(
    longitude = NULL,
    latitude = NULL,
    SFDF = NULL,
    useBuffering = FALSE,
    verbose = FALSE
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(longitude)
  MazamaCoreUtils::stopIfNull(latitude)
  MazamaCoreUtils::stopIfNull(SFDF)
  MazamaCoreUtils::stopIfNull(useBuffering)
  MazamaCoreUtils::stopIfNull(verbose)

  # Sanity check -- same number of latitudes and longitudes
  if ( length(longitude) != length(latitude) )
    stop("Arguments 'longitude' and 'latitude' must have the same length.")

  MazamaCoreUtils::validateLonsLats(longitude, latitude, na.rm = TRUE)

  # ----- Get the data ---------------------------------------------------------

  # Determine which longitude/latitude pairs are non-missing
  validLocationIndices <- intersect(which(!is.na(longitude)), which(!is.na(latitude)))

  # Create an sf object
  location <-
    dplyr::tibble(
      lon = longitude[validLocationIndices],
      lat = latitude[validLocationIndices]
    ) %>%
    sf::st_as_sf(coords = c("lon", "lat")) %>%
    # Assign lon-lat North America projection: https://epsg.io/4269
    sf::st_set_crs(4269)

  # Intersect to retrieve SFDF records
  polygonIndex <- sf::st_intersects(location, SFDF) %>% as.numeric()
  validDF <- SFDF[polygonIndex,]

  # Find the locationIndex of the locations where no intersection was found
  badLocationsIndex <- which(is.na(validDF$countryCode))

  # ----- Use buffering --------------------------------------------------------

  # If there are no bad locations, this block is skipped

  # If locations are found which do not intersect with any polygon, increment
  # search for polygons with an increasing radius until the limit is reached or
  # a country is found.

  if ( (length(badLocationsIndex) != 0) && useBuffering ) {

    if (verbose)
      print(paste0(length(badLocationsIndex),
                   ' locations were outside of all polygons -- begin buffering ...'))

    # Sets radius values (in meters) in roughly logarithmic increases
    searchRadii <- c(1000, 2000, 5000, 10000, 20000, 50000, 100000, 200000)

    # Loop over locations of interest, trying to find an intersecting polygon
    for ( locationIndex in badLocationsIndex ) {

      if (verbose)
        print(paste0('locationIndex = ', locationIndex))

      # Select the individual location we are analyzing
      locationOfInterest <- location[locationIndex,]

      for ( radius in searchRadii ) {

        if (verbose)
          print(paste0('Using radius = ', radius,
                       ' to search through ', nrow(SFDF),
                       ' polygons ...'))

        # NOTE:  Need to handle returns of type list with the following message:
        #   "Sparse geometry binary predicate list of length 1, where the predicate was `is_within_distance'"
        polygonIndex <-
          sf::st_is_within_distance(locationOfInterest, SFDF, radius)

        if ( "list" %in% class(polygonIndex) ) {
          # Just choose the first one
          polygonIndex <- unlist(polygonIndex)[1] %>% as.numeric()
        } else {
          polygonIndex <- as.numeric(polygonIndex)
        }

        if ( is.na(polygonIndex) ) {

          # Keep searching
          next

        } else {

          # Done searching
          validDF[locationIndex,] <- SFDF[polygonIndex,]
          break

        }

      } # END of ( radius in searchRadii )

    } # END of ( locationIndex in badLocationsIndex )

  } # END of ( use buffering )

  # ----- Assemble return dataframe --------------------------------------------

  # Create a data frame for all locations, valid and not valid
  locationsDF <- data.frame(matrix(NA, ncol = ncol(validDF), nrow = length(longitude)))
  colnames(locationsDF) <- colnames(SFDF)

  # Place the valid locations in their correct position in locationsDF
  for (i in seq_along(validLocationIndices)) {
    locationsDF[validLocationIndices[i],] <- validDF[i,]
  }

  # Drop the geometry column to return a non-spatial dataframe
  locationsDF <- locationsDF %>% sf::st_drop_geometry()
  # TODO: Figure out why st_drop_geometry() isn't working or post an issue
  if ( "geometry" %in% names(locationsDF) )
    locationsDF$geometry <- NULL

  # ----- Return ---------------------------------------------------------------

  return(locationsDF)

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {

  library(MazamaSpatialUtils)
  setSpatialDataDir("~/Data/Spatial_0.8")
  loadSpatialData("USCensusStates_02")

  SFDF <- USCensusStates_02
  useBuffering <- TRUE
  verbose <- TRUE

  longitude <- -120:-105
  latitude <- 40:55



  bop <- getSpatialData(longitude, latitude, SFDF)



}
