#' @keywords locator
#' @export
#'
#' @title Get polygonID from SFDF of interest
#'
#' @param SFDF Spatial polygons dataset of interest.
#'
#' @description Extracts the the vector of unique polygon identifiers from
#' \code{SFDF}.
#'
#' This function is useful when writing code to aggregate data by polygon and
#' calculate per-polygon statistics. Each unique simple features data frame will
#' have a different set of data columns but each is guaranteed to have a column
#' named \code{polygonID} that uniquely identifies each polygon.
#'
#' This allows us to write code that aggregates by polygon without having to
#' know whether the polygons represent, countries, timezones or HUCs, etc.
#'
#' @return Vector of polygon identifiers.
getPolygonID <- function(SFDF) {

  if ( !"sf" %in% class(SFDF) ) {

    stop(deparse(substitute(SFDF)),
         ' is not a simple features data frame.',
         call. = FALSE)

  } else {

    return(SFDF$polygonID)

  }

}

