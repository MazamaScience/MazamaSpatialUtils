#' @keywords datagen
#' @export
#' @title Convert OSM Timezone Shapefile 
#' @param dsnPath optional directory where the timezones.shapefile.zip file is found (in case web access isn't working)
#' @param nameOnly logical specifying whether to only return the name without creating the file
#' @description A world timezone shapefile is downloaded from \url{https://github.com/evansiroky/timezone-boundary-builder/releases}
#' and converted to a SpatialPolygonsDataFrame with additional columns of data. The resulting file will be created
#' in the spatial data directory which is set with \code{setSpatialDataDir()}.
#' @note
#' There are 86 timezones which have polygons but the associated rows in the dataframe have no data. 
#' These timezones also have no \code{countryCode} assigned. We hope to rectify this in a future release. 
#' These are the missing timezones:
#' \preformatted{
#' > OSMTimezones@data$timezone[is.na(OSMTimezones$countryCode)]
#'  [1] "Africa/Addis_Ababa"    "Africa/Asmara"         "Africa/Bamako"         "Africa/Bangui"         "Africa/Banjul"        
#'  [6] "Africa/Blantyre"       "Africa/Brazzaville"    "Africa/Bujumbura"      "Africa/Conakry"        "Africa/Dakar"         
#' [11] "Africa/Dar_es_Salaam"  "Africa/Djibouti"       "Africa/Douala"         "Africa/Freetown"       "Africa/Gaborone"      
#' [16] "Africa/Harare"         "Africa/Juba"           "Africa/Kampala"        "Africa/Kigali"         "Africa/Kinshasa"      
#' [21] "Africa/Libreville"     "Africa/Lome"           "Africa/Luanda"         "Africa/Lubumbashi"     "Africa/Lusaka"        
#' [26] "Africa/Malabo"         "Africa/Maseru"         "Africa/Mbabane"        "Africa/Mogadishu"      "Africa/Niamey"        
#' [31] "Africa/Nouakchott"     "Africa/Ouagadougou"    "Africa/Porto-Novo"     "Africa/Sao_Tome"       "America/Anguilla"     
#' [36] "America/Antigua"       "America/Aruba"         "America/Cayman"        "America/Coral_Harbour" "America/Dominica"     
#' [41] "America/Grenada"       "America/Guadeloupe"    "America/Kralendijk"    "America/Lower_Princes" "America/Marigot"      
#' [46] "America/Montreal"      "America/Montserrat"    "America/St_Barthelemy" "America/St_Kitts"      "America/St_Lucia"     
#' [51] "America/St_Thomas"     "America/St_Vincent"    "America/Tortola"       "Arctic/Longyearbyen"   "Asia/Aden"            
#' [56] "Asia/Bahrain"          "Asia/Chongqing"        "Asia/Harbin"           "Asia/Kashgar"          "Asia/Kuwait"          
#' [61] "Asia/Muscat"           "Asia/Phnom_Penh"       "Asia/Rangoon"          "Asia/Vientiane"        "Atlantic/St_Helena"   
#' [66] "Europe/Bratislava"     "Europe/Busingen"       "Europe/Guernsey"       "Europe/Isle_of_Man"    "Europe/Jersey"        
#' [71] "Europe/Ljubljana"      "Europe/Mariehamn"      "Europe/Podgorica"      "Europe/San_Marino"     "Europe/Sarajevo"      
#' [76] "Europe/Skopje"         "Europe/Vaduz"          "Europe/Vatican"        "Europe/Zagreb"         "Indian/Antananarivo"  
#' [81] "Indian/Comoro"         "Indian/Mayotte"        "Pacific/Johnston"      "Pacific/Midway"        "Pacific/Saipan"       
#' [86] "Pacific/Yap"     
#' }
#' @return Name of the dataset being created.
#' @references \url{https://github.com/evansiroky/timezone-boundary-builder/releases}
#' @seealso setSpatialDataDir
#' @seealso convertWikipediaTimezoneTable
convertOSMTimezones <- function(dsnPath=NULL, nameOnly=FALSE) {
  
  # Use package internal data directory
  dataDir <- getSpatialDataDir()
  
  # Specify the name of the file being created
  datasetName <- 'OSMTimezones'
  
  if (nameOnly) return(datasetName)
  
  # Build appropriate request URL for world timezones
  url <- "https://github.com/evansiroky/timezone-boundary-builder/releases/download/2017a/timezones.shapefile.zip"

  filePath <- paste(dataDir,basename(url),sep='/')
  
  # NOTE:  "403 Forbidden" when downloading automatically. Support manually downloaded file.
  if ( is.null(dsnPath) ) {
    utils::download.file(url,filePath)
  } else {
    utils::unzip(filePath,exdir=dataDir)
  }
  
  # Convert shapefile into SpatialPolygonsDataFrame
  dsnPath <- paste(dataDir,'dist',sep='/')
  SPDF <- convertLayer(dsn=dsnPath,layerName='combined_shapefile')
  
  # Rename "TZID" to "timezone"
  names(SPDF@data) <- c('timezone')
  
  # Now get additional data from Wikipedia
  wikipediaTimezoneTable <- convertWikipediaTimezoneTable()
  
  # Merge the additional data onto the @data slot of the SPDF
  SPDF@data <- dplyr::left_join(SPDF@data, wikipediaTimezoneTable, by="timezone")
  
  # Group polygons with the same identifier
  SPDF <- organizePolygons(SPDF, uniqueID='timezone')
  
  # Create a simplified version at 5%
  SPDF_05 <- rmapshaper::ms_simplify(SPDF, .05)
  SPDF_05@data$rmapshaperid <- NULL
  datasetName_05 <- paste0(datasetName, "_05")
  
  # Assign a name and save the data
  assign(datasetName,SPDF)
  assign(datasetName_05, SPDF_05)
  save(list=c(datasetName),file=paste0(dataDir,'/',datasetName,'.RData'))
  save(list=c(datasetName_05), file=paste0(dataDir,'/',datasetName_05,'.RData'))
  
  # Clean up
  unlink(filePath, force=TRUE)
  unlink(dsnPath, recursive=TRUE, force=TRUE)
  
  return(invisible(datasetName))
}

