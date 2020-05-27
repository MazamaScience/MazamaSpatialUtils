#' @keywords internal
#' @export
#' @title Convert Shapefile Layer to Spatial Polygon Dataframe
#' @param dsn dsn argument to readOGR
#' @param layerName layer argument to readOGR
#' @param encoding encoding string (.e.g. 'latin1') passed to rgdal::readOGR()
#' @description Raw shapefiles are read in using the \code{rgdal::readOGR()} function from the \pkg{rgdal} package.
#' Spatial data are reprojected onto a standard projection with a CRS class of
#' \code{"+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"} before being returned.
#'
#' If shapefiles have no projection information they are assumed to 'geographic' coordinates  and will be assigned
#' \code{"+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"} before being returned.
#' @return An object of class \code{SpatialPolygonsDataFrame}
convertLayer <- function(
  dsn = "", 
  layerName = "", 
  encoding = NULL
) {
  
  # readOGR does not interpret '~' so do that with path.expand()
  dsn <- path.expand(dsn)
  
  # NOTE:  Chosoe 'use_iconv' setting based on encoding (see ?rgdal::readOGR)

  # Read in projected data
  data_projected <- rgdal::readOGR(
    dsn = dsn, 
    layer = layerName, 
    stringsAsFactors = FALSE, 
    encoding = encoding,
    use_iconv = ifelse(is.null(encoding), FALSE, TRUE)
  ) 
  
  # Assign or reproject to standard projection
  if ( is.na(sp::proj4string(data_projected)) ) {
    # NOTE:  shapefiles used in convertHMSSmoke() have no projection whatsoever
    SPDF <- data_projected
    SPDF@proj4string <- sp::CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
  } else {
    SPDF <- sp::spTransform(data_projected, sp::CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"))
  }
  
  return(SPDF)
}

