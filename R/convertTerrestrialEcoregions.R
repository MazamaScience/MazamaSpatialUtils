#' @keywords datagen
#' @export
#' @title Convert Terrestrial Ecoregion Shapefile
#' @param nameOnly logical specifying whether to only return the name without creating the file
#' @param simplify logical specifying whether to create a "_05" version of the file that is simplified to 5\%
#' @description A shapefile is downloaded and converted to a SpatialPolygonsDataFrame
#'  with additional columns of data. The resulting file will be created
#' in the spatial data directory which is set with \code{setSpatialDataDir()} 
#' @references \url{https://www.worldwildlife.org/publications/terrestrial-ecoregions-of-the-world}
#' @return Name of the dataset being created.

convertTerrestrialEcoregions <- function(nameOnly=FALSE, simplify=TRUE) {
  
  # Use package internal data directory
  dataDir <- getSpatialDataDir()
  
  # Specify the name of the file being created
  datasetName <- 'TerrestrialEcoregions'
  
  if (nameOnly) return(datasetName)
  
  # Build appropriate request URL for terrestrial ecoregions
  url <- "https://c402277.ssl.cf1.rackcdn.com/publications/15/files/original/official_teow.zip?1349272619"
  
  filePath <- file.path(dataDir,'official_teow.zip')
  utils::download.file(url,filePath)
  utils::unzip(filePath,exdir=dataDir)
  
  # Convert shapefile into SpatialPolygonsDataFrame
  dsnPath <- file.path(dataDir,'official')
  SPDF <- convertLayer(dsn=dsnPath,layerName='wwf_terr_ecos')
  
  # > names(SPDF@data)
  # [1] "OBJECTID"   "AREA"       "PERIMETER"  "ECO_NAME"   "REALM"      "BIOME"      "ECO_NUM"    "ECO_ID"    
  # [9] "ECO_SYM"    "GBL_STAT"   "G200_REGIO" "G200_NUM"   "G200_BIOME" "G200_STAT"  "Shape_Leng" "Shape_Area"
  # [17] "area_km2"   "eco_code"   "PER_area"   "PER_area_1" "PER_area_2"
  
  usefulColumns <- c("AREA", "ECO_NAME", "REALM", "BIOME", "ECO_NUM", "ECO_ID", "ECO_SYM", "GBL_STAT", 
                     "G200_REGIO", "G200_NUM", "G200_BIOME", "G200_STAT", "eco_code")
  
  SPDF <- SPDF[usefulColumns]
  names(SPDF) <- c("area", "ecoregionName", "realm", "biome", "ecoregionNumber", "ecoregionID", "ECO_SYM", 
                   "GBL_STAT", "G200Region", "G200Number", "G200Biome", "G200Stat", "ecoregionCode")
  
  # ecoregionName, ecoregionID, and ecoregionCode are all equivalent identifiers for each ecoregion
  
  # convert area to m^2
  SPDF$area <- as.numeric(SPDF$area)
  SPDF$area <- SPDF$area*1000000
  
  # Get latitude and longitude from polygon centroids 
  centroids <- rgeos::gCentroid(SPDF, byid=TRUE)
  lon <- sp::coordinates(centroids)[,1]
  lat <- sp::coordinates(centroids)[,2]
  
  SPDF$longitude <- lon
  SPDF$latitude <- lat
  
  # Get countryCode from latitude and longitude 
  SPDF$countryCode <- getCountryCode(SPDF$longitude, SPDF$latitude, useBuffering=FALSE)

  # Assign country and state codes to polygons where it is missing:
  ecoregionMode <- function(x) {
    if ( sum(!is.na(x)) == 0 ){
      return(NA)
    } else {
      table <- table(x)
      names(which.max(table))
    }
  }
  locations <- data.frame(ecoregionCode = unique(SPDF$ecoregionCode), 
                          countryCode = NA)
  rownames(locations) <- unique(SPDF$ecoregionCode)
  for ( i in unique(SPDF$ecoregionCode) ) {
    ecoregion <- subset(SPDF@data, SPDF$ecoregionCode == i)
    if ( nrow(ecoregion) == 1 ) {
      locations[i,2] <- c(ecoregion$countryCode)
    } else {
      locations[i,2] <- ecoregionMode(ecoregion$countryCode)
    }
    ecoregion <- NULL
  }
  
  # Group polygons with the same identifier
  SPDF <- organizePolygons(SPDF, uniqueID = "ecoregionCode", sumColumns = "area")
  
  # Reassign countries and states
  SPDF@data[,c("countryCode")] <- locations[SPDF$ecoregionCode, c("countryCode")]
  
  # Assign a name and save the data
  assign(datasetName,SPDF)
  save(list=c(datasetName),file=paste0(dataDir,'/',datasetName,'.RData'))

  if ( simplify ) {
    # Create a simplified version at 5%
    SPDF_05 <- rmapshaper::ms_simplify(SPDF, .05)
    SPDF_05@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
    datasetName_05 <- paste0(datasetName, "_05")
    
    # Assign a name and save the data
    assign(datasetName_05, SPDF_05)
    save(list=c(datasetName_05), file=paste0(dataDir,'/',datasetName_05,'.RData'))
  }
  
  # Clean up
  unlink(filePath, force=TRUE)
  unlink(dsnPath, recursive=TRUE, force=TRUE)
  
  return(invisible(datasetName))
}

