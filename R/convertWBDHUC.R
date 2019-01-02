#' @keywords datagen
#' @export
#' @title Convert USGS hydrologic unit shapefiles
#' @param dsnPath directory where the WBD HUC datasets are found
#' @param level Character or integer which must be 2, 4, 6, 8, 10, 12 or 14.
#' @param extension Character extension associated with mapshaper simplified files.
#' @param nameOnly Logical specifying whether to only return the name without 
#' creating the file.
#' @param simplify Logical specifying whether to create "_02" and "_01" versions 
#' of the file that are simplified to 2\% and 1\%.
#' @description Previously downloaded shapefiles from the USGS 
#' \href{http://nhd.usgs.gov/wbd.html}{Watershed Boundary Dataset} are converted to a 
#' SpatialPolygonsDataFrame with additional columns of data. The resulting file will be
#' created in the spatial data directory which is set with \code{setSpatialDataDir()}.
#' @details The full WBD dataset can be downloaded from the USGS with the 
#' following command:
#' \preformatted{
#' curl https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/WBD/National/GDB/WBD_National_GDB.zip -O
#' }
#' 
#' 
#' Typically, the raw data will be simplified using 
#' \href{https://github.com/mbloch/mapshaper}{mapshaper}.
#' 
#' With mapshaper, you can reduce the number of vertices in the polygons, greatly improving
#' the efficiency of spatial searches. Experimentation shows that a reduction to 1-2%
#' of the original shapefile size still retains the recognizable shape of polygons, removing
#' only the higher order "crenellations" in the polygons.
#' 
#' An example use of mapshaper would be:
#' \preformatted{
#' mapshaper WBDHU2.shp --simplify 1% --o WBDHU2_01.shp
#' }
#' 
#' A full suite of \code{.shp, .shx, .dbf, .prj} files will be created for the new name \code{WBDHU2_02}.
#' 
#' @return Name of the dataset being created.
#' @references \url{http://nhd.usgs.gov/wbd.html}
#' @seealso setSpatialDataDir

# TODO:  Convert missing state codes to state codes from allStateCodes, with a note explaining 
# TODO:  how and why. Figure out why it is printing all those numbers when it runs and change.  


convertWBDHUC <- function(dsnPath=NULL, level=8, extension="", nameOnly=FALSE, simplify=FALSE) {
  
  # Sanity check dsnPath
  if ( is.null(dsnPath) ) stop(paste0('Argument dsnPath must be specified.'))
  if ( !file.exists(dsnPath) ) stop(paste0('dsnPath="',dsnPath,'" not found.'))
  
  # 'level' should be a character string
  level <- as.character(level)
  
  # Use package internal data directory
  dataDir <- getSpatialDataDir()
  
  # Specify the name of the dataset and file being created
  datasetName <- paste0('WBDHU', level) 
  
  if (nameOnly) return(datasetName)

  # Convert shapefile into SpatialPolygonsDataFrame
  layerName <- paste0('WBDHU', level, extension)
  message("Reading in data...\n")
  SPDF <- convertLayer(dsn=dsnPath, layerName=layerName)

  # Rationalize naming:
  # * human readable full nouns with descriptive prefixes
  # * generally lowerCamelCase
  # with internal standards:
  # * countryCode (ISO 3166-1 alpha-2)
  # * stateCode (ISO 3166-2 alpha-2)
  # * longitude (decimal degrees E)
  # * latitude (decimal degrees N)
  
  # Subset this dataframe to include only obviously useful columns
  
  # NOTE:  Comments are relevant to the WBD as downlaoded on 2017-07-12
  
  if ( level == '2' ) {
    # 22 features
    #
    # > names(SPDF)
    # [1] "TNMID"            "METASOURCEID"     "SOURCEDATADESC"   "SOURCEORIGINATOR"
    # [5] "SOURCEFEATUREID"  "LOADDATE"         "GNIS_ID"          "AREAACRES"       
    # [9] "AREASQKM"         "STATES"           "HUC2"             "NAME"            
    # [13] "GLOBALID"         "SHAPE_Length"     "SHAPE_Area"      
    
    usefulColumns <- c('LOADDATE','GNIS_ID','AREASQKM', 'HUC2', 'NAME', 'STATES')
  } else if ( level == '4' ) {
    # 223 features
    # 
    # [1] "TNMID"            "METASOURCEID"     "SOURCEDATADESC"   "SOURCEORIGINATOR" "SOURCEFEATUREID"  "LOADDATE"        
    # [7] "GNIS_ID"          "AREAACRES"        "AREASQKM"         "STATES"           "HUC4"             "NAME"            
    # [13] "GLOBALID"         "SHAPE_Length"     "SHAPE_Area"     
    # 
    usefulColumns <- c('LOADDATE','GNIS_ID','AREASQKM', 'HUC4', 'NAME', 'STATES')
  } else if ( level == '6' ) {
    # 387 features
    #
    # [1] "TNMID"            "METASOURCEID"     "SOURCEDATADESC"   "SOURCEORIGINATOR" "SOURCEFEATUREID"  "LOADDATE"        
    # [7] "GNIS_ID"          "AREAACRES"        "AREASQKM"         "STATES"           "HUC6"             "NAME"            
    # [13] "GLOBALID"         "SHAPE_Length"     "SHAPE_Area"      
    usefulColumns <- c('LOADDATE','GNIS_ID','AREASQKM', 'HUC6', 'NAME', 'STATES')
  } else if (level == '8' ) {
    # 2309 features
    #
    # [1] "TNMID"            "METASOURCEID"     "SOURCEDATADESC"   "SOURCEORIGINATOR" "SOURCEFEATUREID"  "LOADDATE"        
    # [7] "GNIS_ID"          "AREAACRES"        "AREASQKM"         "STATES"           "HUC8"             "NAME"            
    # [13] "GLOBALID"         "SHAPE_Length"     "SHAPE_Area"      
    usefulColumns <- c('LOADDATE','GNIS_ID','AREASQKM', 'HUC8', 'NAME', 'STATES')
  } else if (level == '10' ) {
    # 18514 features
    # 
    #  [1] "TNMID"            "METASOURCEID"     "SOURCEDATADESC"   "SOURCEORIGINATOR" "SOURCEFEATUREID"  "LOADDATE"        
    # [7] "GNIS_ID"          "AREAACRES"        "AREASQKM"         "STATES"           "HUC10"            "NAME"            
    # [13] "HUTYPE"           "HUMOD"            "GLOBALID"         "SHAPE_Length"     "SHAPE_Area"   
    usefulColumns <- c('LOADDATE','GNIS_ID','AREASQKM', 'HUC10', 'NAME', 'STATES')
  } else if (level == '12' ) {
    # 101064 features
    # TODO:  Figure out what NONCONTRIBUTINGAREA variables are and what to do about them.
    #
    # [1] "TNMID"                    "METASOURCEID"             "SOURCEDATADESC"           "SOURCEORIGINATOR"        
    # [5] "SOURCEFEATUREID"          "LOADDATE"                 "GNIS_ID"                  "AREAACRES"               
    # [9] "AREASQKM"                 "STATES"                   "HUC12"                    "NAME"                    
    # [13] "HUTYPE"                   "HUMOD"                    "TOHUC"                    "NONCONTRIBUTINGAREAACRES"
    # [17] "NONCONTRIBUTINGAREASQKM"  "GLOBALID"                 "SHAPE_Length"             "SHAPE_Area"              
    usefulColumns <- c('LOADDATE','GNIS_ID','AREASQKM', 'HUC12', 'NAME', 'STATES')
  } else if (level == '14' ) {
    # NOTE:  On 2017-04-13 it looks like the WBDHU14 file only contains HUCs for southeast Alaska
    # 7113 features
    #
    # [1] "TNMID"                    "METASOURCEID"             "SOURCEDATADESC"           "SOURCEORIGINATOR"        
    # [5] "SOURCEFEATUREID"          "LOADDATE"                 "GNIS_ID"                  "AREAACRES"               
    # [9] "AREASQKM"                 "STATES"                   "HUC14"                    "NAME"                    
    # [13] "HUTYPE"                   "HUMOD"                    "NONCONTRIBUTINGAREAACRES" "NONCONTRIBUTINGAREASQKM" 
    # [17] "GLOBALID"                 "SHAPE_Length"             "SHAPE_Area"     
    
    usefulColumns <- c('LOADDATE','GNIS_ID','AREASQKM', 'HUC14', 'NAME', 'STATES')
  }
  
  
  SPDF <- SPDF[,usefulColumns]
  names(SPDF) <- c('loadDate','GNISCode','area','HUC','HUCName', 'allStateCodes')

  # Change are from km^2 to m^2
  SPDF@data$area <- as.numeric(SPDF@data$area) * 1000000
  
  # Group polygons with duplicated hydrologic unit codes
  # NOTE:  The USGS WBD polygons seem to be well organized
  if ( length(SPDF@polygons) != nrow(SPDF@data) ) {
    message("Organizing polygons...\n")
    SPDF <- organizePolygons(SPDF, uniqueID='HUC', sumColumns='area')
  }

  # TODO:  Larger HUCs are centered in the US, while at smaller levels the entire
  # TODO:  HUCs are in foreign countries (ie Canada). Find a way to eliminate smaller
  # TODO:  HUCs whose 'allStateCodes' is not a US State

  # Calculate centroids to help add more metadata
  result <- try( {
    message("Calculating centroids...\n")
    centroids <- rgeos::gCentroid(SPDF, byid=TRUE)
    lon <- sp::coordinates(centroids)[,1]
    lat <- sp::coordinates(centroids)[,2]
  }, silent=TRUE)
  
  # NOTE:  This failed for a simplified version of HU10 with:
  # NOTE:
  # NOTE:  Error in createPolygonsComment(p) : 
  # NOTE:    rgeos_PolyCreateComment: orphaned hole, cannot find containing polygon for hole at index 147
  # NOTE:
  # NOTE:  If centroids don't work we'll just default to the center of the bbox for each polygon
  
  if ( class(result)[1] == "try-error" ) {
    warning('NOTE: rgeos::gCentroid() failed with the following message. Using bbox() to calculate lon and lat.\n')
    warning(geterrmessage(),'\n')
    lon <- rep(as.numeric(NA), nrow(SPDF))
    lat <- rep(as.numeric(NA), nrow(SPDF))
    for (i in seq_len(nrow(SPDF)) ) {
      bbox <- sp::bbox(SPDF[i,])
      lon[i] <- mean(bbox[1,])
      lat[i] <- mean(bbox[2,])
    }
  }
  
  # Add more standard columns
  SPDF$longitude <- lon
  SPDF$latitude <- lat  
  SPDF$countryCode <- 'US'
  
  #NOTE: this takes quite a long time. 
  message("Getting stateCode...\n")
  suppressWarnings(SPDF$stateCode <- getStateCode(lon, lat, countryCodes=c('US')))
   
  # Hack to change missing stateCodes to the value from allStateCodes
  
  for (i in seq_len(nrow(SPDF)) ){
    if (is.na(SPDF@data$stateCode[i])){
      SPDF@data$stateCode[i] <- SPDF@data$allStateCodes[i]      
    }
    if (stringr::str_length(SPDF@data$stateCode[i]) > 2){
      SPDF@data$stateCode[i] <- substr(SPDF@data$stateCode[i], start=1, stop=2)
    }
  }
  
  
  SPDF$polygonID <- SPDF$HUC
  
  # Assign a name and save the data
  message("Saving full resolution version...\n")
  assign(datasetName,SPDF)
  save(list=datasetName, file = paste0(dataDir,"/",datasetName, '.RData'))
  rm(list=datasetName)
  
  if ( simplify ) {
    # Create two new simplified datsets: one with 2%, and one with 1% of the vertices of the original
    # NOTE:  This may take several minutes. 
    message("Simplifying to 2%...\n")
    SPDF_02 <- rmapshaper::ms_simplify(SPDF, 0.02)
    SPDF_02@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
    datasetName_02 <- paste0(datasetName, "_02")
    message("Saving 2% version...\n")
    assign(datasetName_02, SPDF_02)
    save(list=datasetName_02, file = paste0(dataDir,"/",datasetName_02, '.RData'))
    rm(list=c("SPDF_02",datasetName_02))
    
    message("Simplifying to 1%...\n")
    SPDF_01 <- rmapshaper::ms_simplify(SPDF, 0.01) 
    SPDF_01@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
    datasetName_01 <- paste0(datasetName, "_01")
    message("Saving 1% version...\n")
    assign(datasetName_01, SPDF_01)
    save(list=datasetName_01, file = paste0(dataDir,"/",datasetName_01, '.RData'))
    rm(list=c("SPDF_01",datasetName_01))
  }
  
  return(invisible(datasetName))
  
}

# # NOTE:  To generate all levels:
# if ( FALSE ) {
# 
#   for ( i in c(2,4,6) ) {
#     message("----- Processing level ",i," -----\n")
#     convertWBDHUC(dsnPath="~/Data/SpatialRaw/WBD.gdb", level=i, simplify=TRUE)
#   }
# 
#   # NOTE:  Running out of memory trying to simplify level 8 or above
#   for ( i in c(8,10,12) ) {
#     message("----- Processing level ",i," -----\n")
#     convertWBDHUC(dsnPath="~/Data/SpatialRaw/WBD.gdb", level=i, simplify=FALSE)
#   }
#   
# }

