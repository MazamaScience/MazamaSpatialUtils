#' @keywords datagen
#' @export
#' @title Convert US State Legislative Districts Shapefile
#' @param stateCode ISO 3166-2 alpha-2 state code
#' @param house character specifing either "Upper" or "Lower" house
#' @param nameOnly logical specifying whether to only return the name without creating the file
#' @description  a SpatialPolygonsDataFrame for US State Legislative Districts
#' @details A US State Legislative District shapefile is downloaded and converted to a
#' SpatialPolygonsDataFrame with additional columns of data. The resulting file will be created
#' in the spatial data directory which is set with \code{setSpatialDataDir()}.
#' @return Name of the dataset being created.
#' @references \url{https://www.census.gov/geo/maps-data/data/cbf/cbf_sld.html}
#' @seealso setSpatialDataDir

convertStateLegislativeDistricts <- function(stateCode, house="Upper", nameOnly=FALSE) {
  
  # Use package internal data directory
  dataDir <- getSpatialDataDir()
  
  # Create dataset name
  datasetName <- paste0(stateCode, house, "HouseLegislativeDistrict")
  if (nameOnly) return(datasetName)
  
  # Check that NaturalEarthAdm1 has been loaded
  if (!exists("NaturalEarthAdm1")) {
    stop("NaturalEarthAdm1 needed. Please load it with loadSpatialData(\"NaturalEarthAdm1\")")
  }
  
  # Create state code dataframe for input validation
  # Keep stateCode and FIPS code for request url construction
  stateCodeDF <- dplyr::select(dplyr::filter(NaturalEarthAdm1@data, countryCode == 'US'),
                               stateCode, fips)
  
  # Check that stateCode is a valid state code
  if (!(stateCode %in% stateCodeDF[["stateCode"]])) {
    stop(paste(stateCode, "is not a valid stateCode. Please try again."))
  }
  
  # Check that house is either "Upper" or "Lower"
  if (!(house == "Upper" || house == "Lower")) {
    stop(paste0("\"",house, "\" is an invalid value for house. Please use either house=\"Upper\" or house=\"Lower\""))
  }
  
  # Check that house does not equal "Lower" if stateCode equals DC
  if ((house == "Lower" && stateCode == "DC") || (house == "Lower" && stateCode == "NE")) {
    warning(paste(stateCode, "does not have a Lower house, converting shapefile for Upper house instead"))
    house <- "Upper"
  }
  
  # Get FIPS code for state
  stateFips <- stateCodeDF[stateCodeDF["stateCode"] == stateCode, "fips"]
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
  filePath <- paste(dataDir, basename(url), sep="/")
  utils::download.file(url, filePath)
  utils::unzip(filePath, exdir=paste0(dataDir, "/", datasetName))
  
  # Convert shapefile to SpatialPolygonsDataframe
  dsnPath <- paste(dataDir, datasetName, sep="/")
  shpName <- stringr::str_replace(basename(url), ".zip", "")
  SPDF <- convertLayer(dsn = dsnPath, layerName = shpName)
  
  # > names(SPDF)
  # [1] "STATEFP"  "SLDUST"   "AFFGEOID" "GEOID"    "NAME"     "LSAD"     "LSY"      "ALAND"    "AWATER"
  
  # Drop SLDUST, AFFGEOID, and GEOID
  SPDF@data <- dplyr::select(SPDF@data, STATEFP, NAME, LSAD, LSY, ALAND, AWATER)
  
  # Convert names to adhere to naming standard
  SPDF@data <- dplyr::rename(SPDF@data, stateFIPS = STATEFP)
  SPDF@data <- dplyr::rename(SPDF@data, legislativeDistrict = NAME)
  SPDF@data <- dplyr::rename(SPDF@data, legalStatisticalAreaDescriptionCode = LSAD)
  SPDF@data <- dplyr::rename(SPDF@data, legislativeSessionYear = LSY)
  SPDF@data <- dplyr::rename(SPDF@data, areaLand = ALAND)  # already in m^2
  SPDF@data <- dplyr::rename(SPDF@data, areaWater = AWATER)  # already in m^2
  
  # Add ISO 3166-2 alpha-2 codes
  # create a vector of ISO 3166-2 alpha-2 codes, named by their FIPS code
  stateCodeVector <- stateCodeDF$stateCode
  names(stateCodeVector) <- stateCodeDF$fips
  
  # Add stateCode field
  SPDF$stateCode <- stateCodeVector[paste0("US", SPDF$stateFIPS)]
  
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
