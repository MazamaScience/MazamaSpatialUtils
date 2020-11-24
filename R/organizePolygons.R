#' @keywords internal
#' @export
#'
#' @title Organize ungrouped polygons
#'
#' @param SPDF Object of class SpatialPolygonsDataFrame.
#' @param uniqueID Unique identifier to determine which values are duplicated.
#' @param sumColumns Vector of column names to be summed.
#'
#' @description Determines if the SpatialPolygonsDataFrame is grouped. If ungrouped,
#' function will group duplicated values based on the provided unique identifier.
#'
#' If \code{sumColumns} is NULL and there are multiple rows that aren't
#' duplicated but have the same \code{uniqueID}, the original
#' SpatialPolygonsDataFrame will be returned.
#'
#' @return SpatialPolygonsDataFrame composed of grouped polygons.
#'
organizePolygons <- function(
  SPDF,
  uniqueID,
  sumColumns = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  # Test if the unique identifier is a character string
  if ( !is.character(uniqueID) ) {
    stop(paste0("The uniqueID, \"", uniqueID, "\" must be a character string."),
         call. = FALSE)
  }

  # Test if the SPDF is of class SpatialPolygonsDataFrame
  if ( !class(SPDF) == "SpatialPolygonsDataFrame" ) {
    stop(paste0(SPDF, " not found. Please use loadSpatialData()."),
         call. = FALSE)
  }

  # ----- Check for grouped polygons -------------------------------------------

  # Test if the dataframe already contains grouped polygons. If so,
  # add polygonID and return the dataframe.
  if ( !any(duplicated(SPDF@data[,uniqueID])) ) {
    SPDF@data[,'polygonID'] <- as.character(SPDF@data[,uniqueID])
    rownames(SPDF@data) <- as.character(SPDF@data[,uniqueID])
    # Also useful to add the uniqueID to each individual sp::Polygons object in
    # SPDF@polygons
    for ( i in seq_along(SPDF@polygons) ) {
      SPDF@polygons[[i]]@ID <- as.character(SPDF@data[i,uniqueID])
    }
    return(SPDF)
  }

  # ----- Group polygons by uniqueID -------------------------------------------

  # Determine which values are duplicated and create an updated dataframe
  if ( is.null(sumColumns) ) {
    dupMask <- duplicated(SPDF@data)
  } else {
    dupMask <- duplicated(SPDF@data[,uniqueID])
  }
  nonDups <- SPDF[!dupMask,]

  # Test if there are any rows that have non-duplicated data but no columns
  # specified for summation
  if ( any(duplicated(nonDups@data[,uniqueID])) && is.null(sumColumns) ) {
    message(paste0("There are duplicated ",
                   uniqueID,
                   " rows with different values. ",
                   "Please specify columns to be summed. Returning original dataframe."))
    return(SPDF)
  }

  # Create an empty list to store each list Polygons
  srl <- list()

  # Group polygons based off the unique identifier
  for (i in seq_along(nonDups)) {
    x <- nonDups@data[,uniqueID][i]
    allX <- which(SPDF@data[,uniqueID] == x)

    # Create an emply list to store the Polygons corresponding "x"
    newPolygons <- list()
    for (index in seq_along(allX)) {
      newPolygons[[index]] <- SPDF@polygons[[ allX[index] ]]@Polygons[[1]]
    }

    # Create an object of class Polygons
    polys <- sp::Polygons(newPolygons, x)
    srl[[i]] <- polys

    # If a vector of column names is given, sum up those columns and replace the old row
    if ( !is.null(sumColumns) ) {
      for (j in seq_along(sumColumns)) {
        nonDups@data[ i, sumColumns[j] ] <- sum(SPDF@data[ allX, sumColumns[j] ])
      }
    }
  }

  # ----- Return results ---------------------------------------------------------

  # Create an object SpatialPolygons
  proj4 <- SPDF@proj4string
  SP <- sp::SpatialPolygons(srl, proj4string = proj4)

  # Build a new SpatialPolygonsDataFrame from the dataframe and SpatialPolygons
  rownames(nonDups@data) <- nonDups@data[,uniqueID]
  new_SPDF <- sp::SpatialPolygonsDataFrame(SP, nonDups@data)
  new_SPDF@data[,"polygonID"] <- new_SPDF@data[,uniqueID]

  return(SPDF)

}

