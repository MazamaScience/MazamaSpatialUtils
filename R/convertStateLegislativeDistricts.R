#' @keywords datagen
#' @importFrom rlang .data
#' @export
#' @title Convert US dtate legislative districts shapefile
#' @param stateCode ISO 3166-2 alpha-2 state code.
#' @param house Character specifying either "Upper" or "Lower" house.
#' @param nameOnly Logical specifying whether to only return the name without 
#' creating the file.
#' @return Name of the dataset being created.
#' @description  a SpatialPolygonsDataFrame for US State Legislative Districts
#' @details A US State Legislative District shapefile is downloaded and converted 
#' to a SpatialPolygonsDataFrame with additional columns of data. The resulting 
#' file will be created in the spatial data directory which is set with 
#' \code{setSpatialDataDir()}.
#' @references \url{https://www.census.gov/geo/maps-data/data/cbf/cbf_sld.html}
#' @seealso setSpatialDataDir

convertStateLegislativeDistricts <- function(stateCode,
                                             house="Upper",
                                             nameOnly=FALSE) {
  
  # Use package internal data directory
  dataDir <- getSpatialDataDir()
  
  # Create dataset name
  datasetName <- paste0(stateCode, "_", house, "HouseLegislativeDistricts")
  if (nameOnly) return(datasetName)
  
  stateCodesDF <- get("US_stateCodes")
  
  # Check that stateCode is a valid state code
  if (!(stateCode %in% stateCodesDF[["stateCode"]])) {
    stop(paste(stateCode, "is not a valid stateCode. Please try again."))
  }
  
  # Check that house is either "Upper" or "Lower"
  if (!(house == "Upper" || house == "Lower")) {
    stop(paste0("\"",house, "\" is an invalid value for house. Please use either house=\"Upper\" or house=\"Lower\""))
  }
  
  # Check that house does not equal "Lower" if stateCode equals DC
  if ( (house == "Lower" && stateCode == "DC") || 
       (house == "Lower" && stateCode == "NE") ) {
    warning(paste(stateCode, "does not have a Lower house, converting shapefile for Upper house instead"))
    house <- "Upper"
  }
  
  # Get FIPS code for state
  stateFips <- stateCodesDF[stateCodesDF["stateCode"] == stateCode, "fips"]
  stateFips <- stringr::str_replace(stateFips, "US", "")  # remove "US" at the beginning of FIPS code
  
  # create variable to specify Upper or Lower house in request url
  if (house == "Upper") {
    houseChar <- "u"
  } else {  # house == "Lower"
    houseChar <- "l"
  }
  
  # build request url
  shpName <- paste0("cb_2017_", stateFips, "_sld", houseChar, "_500k")
  url <- paste0("www2.census.gov/geo/tiger/GENZ2017/shp/", shpName, ".zip")
  
  # download and unzip shapefile
  filePath <- file.path(dataDir,basename(url))
  utils::download.file(url, filePath)
  utils::unzip(filePath, exdir=file.path(dataDir, datasetName))
  
  # Convert shapefile to SpatialPolygonsDataframe
  dsnPath <- file.path(dataDir,datasetName)
  shpName <- stringr::str_replace(basename(url), ".zip", "")
  SPDF <- convertLayer(dsn = dsnPath, layerName = shpName)
  
  # > names(SPDF)
  # [1] "STATEFP"  "SLDUST"   "AFFGEOID" "GEOID"    "NAME"     "LSAD"     "LSY"      "ALAND"    "AWATER"

  usefulColumns <- c("STATEFP",  "NAME", "LSAD", "LSY", "ALAND", "AWATER")
  SPDF@data <- SPDF@data[,usefulColumns]
  names(SPDF@data) <- c("stateFIPS", "legislativeDistrict",
                        "legalStatisticalAreaDescriptionCode",
                        "legislativeSessionYear",
                        "areaLand", "areaWater")
  
  # Add stateCode field
  SPDF$stateCode <- stateCode
  
  # Add countryCode field
  SPDF$countryCode <- "US"
  
  # Ensure that areaLand and areaWater are numeric
  SPDF$areaLand <- as.numeric(SPDF$areaLand)
  SPDF$areaWater <- as.numeric(SPDF$areaWater)
  
  # Group polygons with the same identifier (legislativeDistrict)
  SPDF <- organizePolygons(SPDF, uniqueID='legislativeDistrict', 
                           sumColumns=c('areaLand', 'areaWater'))
  
  # Assign name to data and save it
  assign(datasetName, SPDF)
  save(list=c(datasetName),file=paste0(dataDir,'/',datasetName,'.RData'))
  
  # Clean up
  unlink(filePath, force=TRUE)
  unlink(dsnPath, recursive=TRUE, force=TRUE)
  
  return(invisible(datasetName))
}
