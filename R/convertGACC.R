#' @keywords datagen
#' @export
#' @title Convert Geographic Area Coordination Center geojson, as defined by NIFC
#' @param nameOnly logical specifying whether to only return the name without creating the file
#' @param simplify logical specifying whether to create "_05" version of the file that is simplified to 5\% 
#' @description Returns a SpatialPolygonsDataFrame for Geographic Area Coordination Centers (GACCs)
#' @details A GACC shapefile is downloaded and converted to a 
#' SpatialPolygonsDataFrame with additional columns of data. The resulting file will be created
#' in the spatial data directory which is set with \code{setSpatialDataDir()}.
#' @return Name of the dataset being created.
#' @references \url{https://hub.arcgis.com/items/72213d9266eb4aefa4403a1bf21dfd61}
#' @seealso setSpatialDataDir
convertGACC <- function(nameOnly=FALSE, simplify = TRUE) {
  
  # Use package internal data directory
  dataDir <- getSpatialDataDir()
  
  # Specify the name of the dataset and file being created
  datasetName <- 'GACC'
  
  if (nameOnly) return(datasetName)
  
  # Build appropriate request URL for GACC regions dataset
  url <- "https://opendata.arcgis.com/datasets/72213d9266eb4aefa4403a1bf21dfd61_0.geojson"
  
  # Convert read geojson in as shapefile
  SPDF <- geojsonio::geojson_read(url, what = "sp")

  
  # Rationalize naming:
  # * human readable full nouns with descriptive prefixes
  # * generally lowerCamelCase
  # with internal standards:
  # * countryCode (ISO 3166-1 alpha-2)
  # * stateCode (ISO 3166-2 alpha-2)
  # * longitude (decimal degrees E)
  # * latitude (decimal degrees N)
  # * area (m^2)
  
  # Standardize naming in the SpatialPolygonsDataFrame
  
  SPDF@data <- dplyr::select(.data = SPDF@data, 
                             National_GACC_2017 = .data$FID_National_GACC_2017,
                             uniID = .data$Unit_ID,
                             GACCName = .data$GACC_Name, 
                             label = .data$GACC_Label,
                             location = .data$Location, 
                             contactPhone = .data$Contact_Phone,
                             GACC_NWCG_Code = .data$GACC_NWCG_Code)
  
  SPDF$countryCode <- 'US'
  SPDF$stateCode <- stringr::str_extract(SPDF$location, "[[:upper:]]{2}$")
  
  # Assign a name and save the data
  assign(datasetName,SPDF)
  save(list=c(datasetName),file=paste0(dataDir,'/',datasetName,'.RData'))
  
  # simplify
  if ( simplify ) {
    
    SPDF_05 <- rmapshaper::ms_simplify(SPDF, .05)
    datasetName_05 <- paste0(datasetName, "_05")
    assign(datasetName_05, SPDF_05)
    save(list = datasetName_05, file = paste0(dataDir, "/", datasetName_05, ".RData"))
    
  }
  
  return(invisible(datasetName))
}

