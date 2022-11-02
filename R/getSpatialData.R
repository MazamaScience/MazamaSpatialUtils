#' @export
#'
#' @title Return spatial data associated with a set of locations
#'
#' @param longitude Vector of longitudes in decimal degrees East.
#' @param latitude Vector of latitudes in decimal degrees North.
#' @param SFDF Object of class SpatialPolygonsDataFrame.
#' @param useBuffering Logical flag specifying the use of location buffering to
#' find the nearest polygon if no target polygon is found.
#' @param verbose Logical flag controlling detailed progress statements.
#'
#' @return Vector or dataframe of data.
#'
#' @description All locations are first converted to \code{SpatialPoints}
#' objects. The \pkg{sf::st_contains()} function is then used to determine which
#' polygon from \code{SFDF} each location falls in. The dataframe row associated
#' with each polygon is then associated with each location.
#'
#' @details Occasionally for coastal locations the precise coordinates
#' lie outside the boundaries of a low resolution SpatialPolygonsDataFrame.
#' To account for this any location that remains unassociated after the first
#' pass is then buffered to create a small circle around the original location.
#' All polygons are then checked to see if there is any intersection with the
#' now larger buffered locations. Each point is then checked for an intersecting
#' polygon at the following radii: 1km, 2km, 5km, 10km, 20km, 50km, 100km, 200km.
#' If a buffered location is more than 200km away from any polygon, a value of
#' \code{NA} (or data frame row with all \code{NA}s) is returned for that
#' location.
#'
#' Missing or invalid values in the incoming \code{longitude} or \code{latitude} vectors
#' result in \code{NA}s at those positions in the returned vector or data frame.

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
  validIndices <- intersect(which(!is.na(longitude)), which(!is.na(latitude)))
  validPairs <- list(longitude[validIndices], latitude[validIndices])

  # Create the array of locations and use the same projection as SFDF
  locations <- sf::st_point(validPairs)

  # Use the 'over' function to do point-in-polygon searches and extract data
  validDF <- sf::st_contains(SFDF, locations)

  # Find the index of the points where the 'over' function failed to place a
  # coordinate location in a polygon
  badPointsIndex <- which(is.na(validDF$countryCode))

  # TODO:  Enable useBuffering
  locationsDF <- validDF

  # # If NA points are found, increment radius until limit is reached or a country
  # # is found.
  # # If there are no NA points, this block is skipped
  # if ( (length(badPointsIndex) != 0) && useBuffering ) {
  #
  #   if (verbose)
  #     print(paste0(length(badPointsIndex),
  #                  ' points were outside of all polygons -- begin buffering ...'))
  #
  #   # Sets radius values (in meters) in roughly logarithmic increases
  #   searchRadii <- c(1000, 2000, 5000, 10000, 20000, 50000, 100000, 200000)
  #
  #   # Restructures the given SFDF from a SpatialPolygonsDataFrame to a list of
  #   # SpatialPolygons.
  #   # NOTE: We do this restructuring for ease of use later when using the
  #   # 'gIntersects' function
  #   # TODO:  implicit list embedding of S4 objects is depreciated, so we should
  #   # TODO:  find an alternative.
  #   SpatialPolygonsList <- list()
  #   for (i in seq_along(SFDF) ) {
  #     SFDF_Polygons <- SFDF@polygons[[i]]
  #     suppressWarnings({
  #       SpatialPolygonsList[i] <- sp::SpatialPolygons(list(SFDF_Polygons),
  #                                                     proj4string = SFDF@proj4string)
  #     })
  #   }
  #
  #   # Loop over points of interest, trying to find an intersecting polygon
  #   for ( pointIndex in badPointsIndex ) {
  #
  #     if (verbose)
  #       print(paste0('pointIndex = ', pointIndex))
  #
  #     # Select the individual point we are analyzing
  #     pointOfInterest <- location[pointIndex]
  #
  #     for ( radius in searchRadii ) {
  #
  #       if (verbose)
  #         print(paste0('Using radius = ', radius,
  #                      ' to search through ', length(SpatialPolygonsList),
  #                      ' polygons ...'))
  #
  #       # TODO:  Deal with warning messages like:
  #       # TODO:    NULL target CRS comment, falling back to PROJ string
  #       suppressWarnings({
  #
  #         # Switch to a planar projection in order to use 'gBuffer' function
  #         pointOfInterest <- sp::spTransform(
  #           pointOfInterest,
  #           sp::CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  #         )
  #
  #         # Buffer the point
  #         buffer <- rgeos::gBuffer(pointOfInterest, width = radius)
  #
  #         # Transform back to geographical coordinates, exiting the loop if
  #         # -180:180, -90:90 domain boundaries are reached
  #         buffer <- try(sp::spTransform(buffer, SFDF@proj4string), silent = FALSE)
  #
  #       })
  #
  #       if ( class(buffer) == "try-error" )
  #         break
  #
  #       radiusIntersectsPolygon <- FALSE
  #
  #       # Use gIntersects() to determine whether each buffered point is contained
  #       # within each polygon
  #       for ( k in seq_along(SpatialPolygonsList) ) {
  #         if ( rgeos::gIntersects(buffer, SpatialPolygonsList[[k]]) ) {
  #           validDF[pointIndex,] <- SFDF@data[k,]
  #           radiusIntersectsPolygon <- TRUE
  #           break
  #         }
  #       }
  #       # Bail out of searchRadii loop
  #       if (radiusIntersectsPolygon) break
  #
  #     } # END of ( radius in searchRadii )
  #
  #   } # END of ( pointIndex in badPointsIndex )
  #
  # }
  #
  # # Create a data frame for all locations, valid and not valid
  # locationsDF <- data.frame(matrix(NA, ncol = ncol(validDF), nrow = length(longitude)))
  # colnames(locationsDF) <- colnames(validDF)
  # # Place the valid points in their correct position in the locations data frame
  # for (i in seq_along(validIndices)) {
  #   locationsDF[validIndices[i],] <- validDF[i,]
  # }

  return(locationsDF)

}
