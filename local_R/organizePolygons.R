#' @keywords internal
#' @export
#'
#' @title Organize ungrouped polygons
#'
#' @param SFDF Object of class simple features data frame.
#' @param uniqueID Unique identifier to determine which values are duplicated.
#' @param sumColumns Vector of column names to be summed.
#'
#' @description Determines if the simple features data frame is grouped. If ungrouped,
#' function will group duplicated values based on the provided unique identifier.
#'
#' If \code{sumColumns} is NULL and there are multiple rows that aren't
#' duplicated but have the same \code{uniqueID}, the original
#' simple features data frame will be returned.
#'
#' @return simple features data frame composed of grouped polygons.
#'
organizePolygons <- function(
  SFDF,
  uniqueID,
  sumColumns = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  # Test if the unique identifier is a character string
  if ( !is.character(uniqueID) ) {
    stop(paste0("The uniqueID, \"", uniqueID, "\" must be a character string."),
         call. = FALSE)
  }

  # Test if the SFDF is of class simple features data frame
  if ( !class(SFDF) == "simple features data frame" ) {
    stop(paste0(SFDF, " not found. Please use loadSpatialData()."),
         call. = FALSE)
  }

  # ----- Check for grouped polygons -------------------------------------------

  # Test if the dataframe already contains grouped polygons. If so,
  # add polygonID and return the dataframe.
  if ( !any(duplicated(SFDF[,uniqueID])) ) {
    SFDF[,'polygonID'] <- as.character(SFDF[,uniqueID])
    rownames(SFDF) <- as.character(SFDF[,uniqueID])
    # Also useful to add the uniqueID to each individual sp::Polygons object in
    # SFDF@polygons
    for ( i in seq_along(SFDF@polygons) ) {
      SFDF@polygons[[i]]@ID <- as.character(SFDF[i,uniqueID])
    }
    return(SFDF)
  }

  # ----- Group polygons by uniqueID -------------------------------------------

  # Determine which values are duplicated and create an updated dataframe
  if ( is.null(sumColumns) ) {
    dupMask <- duplicated(SFDF)
  } else {
    dupMask <- duplicated(SFDF[,uniqueID])
  }
  nonDups <- SFDF[!dupMask,]

  # Test if there are any rows that have non-duplicated data but no columns
  # specified for summation
  if ( any(duplicated(nonDups@data[,uniqueID])) && is.null(sumColumns) ) {
    message(paste0("There are duplicated ",
                   uniqueID,
                   " rows with different values. ",
                   "Please specify columns to be summed. Returning original dataframe."))
    return(SFDF)
  }

  # Create an empty list to store each list Polygons
  srl <- list()

  # Group polygons based off the unique identifier
  for (i in seq_along(nonDups)) {
    x <- nonDups@data[,uniqueID][i]
    allX <- which(SFDF[,uniqueID] == x)

    # Create an emply list to store the Polygons corresponding "x"
    newPolygons <- list()
    for (index in seq_along(allX)) {
      newPolygons[[index]] <- SFDF@polygons[[ allX[index] ]]@Polygons[[1]]
    }

    # Create an object of class Polygons
    polys <- sp::Polygons(newPolygons, x)
    srl[[i]] <- polys

    # If a vector of column names is given, sum up those columns and replace the old row
    if ( !is.null(sumColumns) ) {
      for (j in seq_along(sumColumns)) {
        nonDups@data[ i, sumColumns[j] ] <- sum(SFDF[ allX, sumColumns[j] ])
      }
    }
  }

  # ----- Return results ---------------------------------------------------------

  # Create an object SpatialPolygons
  proj4 <- SFDF@proj4string
  SP <- sp::SpatialPolygons(srl, proj4string = proj4)

  # Build a new simple features data frame from the dataframe and SpatialPolygons
  rownames(nonDups@data) <- nonDups@data[,uniqueID]
  new_SFDF <- sp::simple features data frame(SP, nonDups@data)
  new_SFDF[,"polygonID"] <- new_SFDF[,uniqueID]

  return(SFDF)

}

