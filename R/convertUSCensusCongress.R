#' @keywords datagen
#' @export
#' @title Convert US Congressional Disctricts Shapefile
#' @param nameOnly logical specifying whether to only return the name without creating the file
#' @description Returns a SpatialPolygonsDataFrame for US Congressional Districts for the 115th US House of Representatives
#' @details A US congressional district shapefile is downloaded and converted to a
#' SpatialPolygonsDataFrame with additional columns of data. The resulting file will be created
#' in the spatial data directory which is set with \code{setSpatialDataDir()}.
#' @return Name of the dataset being created.
#' @references \url{https://www.census.gov/geo/maps-data/data/cbf/cbf_cds.html}
#' @seealso setSpatialDataDir
convertUSCensusCongress <- function(nameOnly=FALSE) {
  
  # Use package internal data directory
  dataDir <- getSpatialDataDir()
  
  # Specify the name of the dataset and file being created
  datasetName <- 'USCensus115thCongress'
  
  if (nameOnly) return(datasetName)
  
  # Build appropriate request URL for US Census Sates data
  url <- 'http://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_cd115_500k.zip'
  
  filePath <- paste(dataDir,basename(url),sep='/')
  utils::download.file(url,filePath)
  # NOTE:  This zip file has no directory so extra subdirectory needs to be created
  utils::unzip(filePath,exdir=paste0(dataDir,'/congress'))
  
  # Convert shapefile into SpatialPolygonsDataFrame
  # NOTE:  The 'states' directory has been created
  dsnPath <- paste(dataDir,'congress',sep='/')
  shpName <- 'cb_2016_us_cd115_500k'
  SPDF <- convertLayer(dsn=dsnPath,layerName=shpName)
  
  # Rationalize naming:
  # * human readable full nouns with descriptive prefixes
  # * generally lowerCamelCase
  # with internal standards:
  # * countryCode (ISO 3166-1 alpha-2)
  # * stateCode (ISO 3166-2 alpha-2)
  # * longitude (decimal degrees E)
  # * latitude (decimal degrees N)
  # * area (m^2)
  
  # Relabel and standardize the naming in the SpatialPolygonsDataFrame
  
  # NOTE:  STATENS is the Geographic Names Information System name for the state polygon
  
  # > names(SPDF)
  # [1] "STATEFP"  "CD115FP"  "AFFGEOID" "GEOID"    "LSAD"     "CDSESSN"  "ALAND"    "AWATER"  
  
  names(SPDF) <- c('stateFIPS','congressionalDistrictFIPS','AFFGeoID','GeoID','LSAD',
                   'CDSession','areaLand','areaWater')
  
  
  # Get stateFIPS conversion table from wikipedia. We need this to find state names and codes
  # from stateFIPS values.
  # URL of S conversions
  url <- 'http://en.wikipedia.org/wiki/Federal_Information_Processing_Standard_state_code'
  
  # Get the raw html from the url
  wikiDoc <- xml2::read_html(url)
  
  # Get a list of tables in the document
  tables <- rvest::html_nodes(wikiDoc, 'table')
  
  # Assume the relevant list is the first table and parse that into a dataframe
  StateTable <- rvest::html_table(tables[[1]])
  
  # Create a vector of stateCodes which are named by their FIPS
  stateCodeVector <- StateTable[["Alpha code"]]
  names(stateCodeVector) <- StateTable[["Numeric code"]]
  
  # Use stateCodeVector to create a stateCode variable
  SPDF$stateCode <- stateCodeVector[as.character(as.numeric(SPDF$stateFIPS))]
  
  # Add countryCode to adhere to the package internal standards
  SPDF$countryCode <- 'US'
  
  # Guarantee that ALAND and AWATER are numeric
  # NOTE:  Areas are already in m^2
  SPDF$areaLand <- as.numeric(SPDF$areaLand)
  SPDF$areaWater <- as.numeric(SPDF$areaWater)
  
  # Group polygons with the same identifier (GeoID)
  SPDF <- organizePolygons(SPDF, uniqueID='GeoID', sumColumns=c('areaLand', 'areaWater'))
  
  # Assign a name and save the data
  assign(datasetName,SPDF)
  save(list=c(datasetName),file=paste0(dataDir,'/',datasetName,'.RData'))
  
  # Clean up
  unlink(filePath, force=TRUE)
  unlink(dsnPath, recursive=TRUE, force=TRUE)
  
  return(invisible(datasetName))
}

