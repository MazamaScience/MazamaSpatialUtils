#' @keywords datagen
#' @export
#' @title Convert Indian Lands Shapefile
#' @param nameOnly logical specifying whether to only return the name without creating the file
#' @description A shapefile is downloaded from \url{https://nationalmap.gov/small_scale/atlasftp.html#indlanp}
#' and converted to a SpatialPolygonsDataFrame with additional columns of data. The resulting file will be created
#' in the spatial data directory which is set with \code{setSpatialDataDir()} 
#' @description This shapefile represents lands administered by the Bureau of Indian Affairs and is compiled by the 
#' National Atlas of the United States of America.
#' at \url{https://nationalmap.gov/small_scale/mld/indlanp.html}.
#' @return Name of the dataset being created.
#' @seealso setSpatialDataDir

convertIndianLands <- function(nameOnly=FALSE) {
  
  # Use package internal data directory
  dataDir <- getSpatialDataDir()
  
  # Specify the name of the file being created
  datasetName <- 'USIndianLands'
  
  if (nameOnly) return(datasetName)
  
  # Build appropriate request URL for terrestrial ecoregions
  url <- "https://prd-tnm.s3.amazonaws.com/StagedProducts/Small-scale/data/Boundaries/indlanp010g.shp_nt00968.tar.gz"
  
  filePath <- paste(dataDir, basename(url),sep='/')
  utils::download.file(url,filePath)
  utils::untar(filePath,exdir=paste0(dataDir, "/indlan"))
  
  # Convert shapefile into SpatialPolygonsDataFrame
  dsnPath <- paste(dataDir,'indlan',sep='/')
  SPDF <- convertLayer(dsn=dsnPath,layerName='indlanp010g')
  
  # Rationalize naming:
  # * human readable full nouns with descriptive prefixes
  # * generally lowerCamelCase
  # with internal standards:
  # * countryCode (ISO 3166-1 alpha-2)
  # * stateCode (ISO 3166-2 alpha-2)
  # * longitude (decimal degrees E)
  # * latitude (decimal degrees N)
  # * area (m^2)
  
  # > names(SPDF@data)
  # [1] "OBJECTID"   "AREA"       "PERIMETER"  "Indlanp010" "FEATURE1"   "GNIS_Name1" "GNIS_ID1"   "ADMIN1"     "FEATURE2"   "GNIS_Name2"
  # [11] "GNIS_ID2"   "ADMIN2"     "FEATURE3"   "GNIS_Name3" "GNIS_ID3"   "ADMIN3"     "URL"        "STATE"      "STATE_FIPS" "ORIG_NAME" 
  # [21] "GIS_ACRES"  "SHAPE_Leng" "SHAPE_Area"
  
  usefulColumns <- c("AREA",  "FEATURE1", "GNIS_Name1", "GNIS_ID1", "ADMIN1", "FEATURE2", "GNIS_Name2", "GNIS_ID2", "ADMIN2", 
                     "FEATURE3", "GNIS_Name3", "GNIS_ID3", "ADMIN3","STATE","STATE_FIPS", "ORIG_NAME")
  SPDF@data <- SPDF@data[usefulColumns]
  names(SPDF@data) <- c("area", "name", "GNISName", "GNISCode", "AdminAgency", "FEATURE2", "GNIS_Name2", "GNIS_ID2", "ADMIN2", 
                     "FEATURE3", "GNIS_Name3", "GNIS_ID3", "ADMIN3","states","FIPS", "ORIG_NAME")
  
  # Change "N/A" to NA
  nafun <- function(x){
    ifelse(x == "N/A", NA, x)
  }
  SPDF@data <- as.data.frame(apply(SPDF@data, 2, nafun))
  
  # Make GNISCode the unique ID. There are several rows where GNISName is empty but GNIS_Name2 is not.
  SPDF$polygonID <- ifelse(is.na(SPDF$GNISCode), SPDF$GNIS_ID2, SPDF$GNISCode) 
  
  # Convert area from square miles to m^2
  SPDF$area <- as.numeric(SPDF$area)
  SPDF$area <- SPDF$area*1609.344^2
  
  # Get latitude and longitude from polygon centroids 
  centroids <- rgeos::gCentroid(SPDF, byid=TRUE)
  lon <- sp::coordinates(centroids)[,1]
  lat <- sp::coordinates(centroids)[,2]
  
  SPDF$longitude <- lon
  SPDF$latitude <- lat
  
  # There are 21 polygons which span more than one state. We can use longitude and latitude to 
  # get one state code for each polygon.
  
  SPDF$stateCode <- getStateCode(SPDF$longitude, SPDF$latitude)
  SPDF$stateName <- getState(SPDF$longitude, SPDF$latitude)
  SPDF$countryCode <- "US"
  
  # Group polygons with the same identifier
  SPDF <- organizePolygons(SPDF, uniqueID = "polygonID", sumColumns = "area")
  
  # Assign a name and save the data
  assign(datasetName,SPDF)
  save(list=c(datasetName),file=paste0(dataDir,'/',datasetName,'.RData'))
  
  # Clean up
  unlink(filePath, force=TRUE)
  unlink(dsnPath, recursive=TRUE, force=TRUE)
  
  return(invisible(datasetName))
}

