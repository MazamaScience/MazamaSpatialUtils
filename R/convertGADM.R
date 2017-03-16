#' @keywords datagen
#' @export
#' @title Convert and Regularize Data from the GADM Database
#' @param nameOnly logical specifying whether to only return the name without creating the file
#' @param countryCode ISO-3166-1 alpha-2 country code
#' @param admLevel administrative level to be downloaded
#' @description A SpatialPolygonsDataFrame file is downloaded from the GADM database with 
#' additional columns of data added. The resulting file will be created in the spatial data 
#' directory which is set with \code{setSpatialDataDir()}. Dataset and file names are generated like this:
#' 
#' \code{paste0('gadm_', countryCode, '_', admLevel)}
#' 
#' Level 0 will return the national outline. Level 1 will give state/province boundaries. etc.
#' @note Not all countries have the same number of levels. Many just have two levels while France has five.
#' @return Name of the dataset being created.
#' @references \url{http://www.gadm.org/country}.
#' @examples
#' \dontrun{
#' convertGADM('DE', 1)
#' }
convertGADM <- function(countryCode=NULL, admLevel=0, nameOnly=FALSE) {
  
  # Use package internal data directory
  dataDir <- getSpatialDataDir()
    
  # Specify the name of the dataset and file being created
  datasetName <- paste0('GADM_', countryCode, '_', admLevel)
  
  if (nameOnly) return(datasetName)

  # Convert 2-character codes into ISO3
  if (stringr::str_length(countryCode) == 2) {
    ISO3 <- codeToCode(countryCode)
  } else {
    stop('The countryCode parameter "',countryCode,'" is not an ISO-3166-1 alpha-2 country code.',call.=FALSE)
  }
    
  # Check if the dataset already exists
  filePath <- paste0(dataDir, '/', datasetName,'.RData')
  if (file.exists(filePath)) {
    return(invisible(datasetName))
  }
  
  # Build appropriate request URL for the GADM Database
  url <- paste0('http://biogeo.ucdavis.edu/data/gadm2.8/rds/',
                ISO3, '_adm',
                admLevel, '.rds')

  # Get the data
  tempfile <- base::tempfile("spatial_data", fileext=".rds")
  utils::download.file(url, tempfile)
  SPDF <- readRDS(tempfile)
  base::file.remove(tempfile)
  
  
  # Rationalize naming:
  # * human readable full nouns with descriptive prefixes
  # * generally lowerCamelCase
  # with internal standards:
  # * countryCode (ISO 3166-1 alpha-2)
  # * stateCode (ISO 3166-2 alpha-2)
  # * longitude (decimal degrees E)
  # * latitude (decimal degrees N)
  
  if (admLevel == 0) {
    
    # > names(SPDF)
    # [1] "OBJECTID"      "ID_0"          "ISO"           "NAME_ENGLISH"  "NAME_ISO"     
    # [6] "NAME_FAO"      "NAME_LOCAL"    "NAME_OBSOLETE" "NAME_VARIANTS" "NAME_NONLATIN"
    # [11] "NAME_FRENCH"   "NAME_SPANISH"  "NAME_RUSSIAN"  "NAME_ARABIC"   "NAME_CHINESE" 
    # [16] "WASPARTOF"     "CONTAINS"      "SOVEREIGN"     "ISO2"          "WWW"          
    # [21] "FIPS"          "ISON"          "VALIDFR"       "VALIDTO"       "POP2000"      
    # [26] "SQKM"          "POPSQKM"       "UNREGION1"     "UNREGION2"     "DEVELOPING"   
    # [31] "CIS"           "Transition"    "OECD"          "WBREGION"      "WBINCOME"     
    # [36] "WBDEBT"        "WBOTHER"       "CEEAC"         "CEMAC"         "CEPLG"        
    # [41] "COMESA"        "EAC"           "ECOWAS"        "IGAD"          "IOC"          
    # [46] "MRU"           "SACU"          "UEMOA"         "UMA"           "PALOP"        
    # [51] "PARTA"         "CACM"          "EurAsEC"       "Agadir"        "SAARC"        
    # [56] "ASEAN"         "NAFTA"         "GCC"           "CSN"           "CARICOM"      
    # [61] "EU"            "CAN"           "ACP"           "Landlocked"    "AOSIS"        
    # [66] "SIDS"          "Islands"       "LDC"          
    
    # NOTE:  Lots of useful potentially useful information here. We will just add the core identifiers
    SPDF$ISO3 <- SPDF$ISO
    SPDF$countryCode <- SPDF$ISO2
    SPDF$countryName <- SPDF$NAME_ENGLISH
    
  } else {
    
    # > names(SPDF)
    # [1] "OBJECTID"  "ID_0"      "ISO"       "NAME_0"    "ID_1"      "NAME_1"    "HASC_1"   
    # [8] "CCN_1"     "CCA_1"     "TYPE_1"    "ENGTYPE_1" "NL_NAME_1" "VARNAME_1"
    
    # NOTE:  Lots of useful potentially useful information here. We will just add the core identifiers
    SPDF$ISO3 <- SPDF$ISO
    SPDF$countryCode <- codeToCode(SPDF$ISO)
    SPDF$countryName <- SPDF$NAME_0
    ### SPDF$stateCode <- 
    SPDF$stateName <- SPDF$NAME_1
    
    # NOTE:  A regular patterm emerges beyond level 1
    # > names(SPDF)
    # [1] "OBJECTID"  "ID_0"      "ISO"       "NAME_0"    "ID_1"      "NAME_1"    "ID_2"     
    # [8] "NAME_2"    "HASC_2"    "CCN_2"     "CCA_2"     "TYPE_2"    "ENGTYPE_2" "NL_NAME_2"
    # [15] "VARNAME_2"
    
    if (admLevel >= 2) {
      SPDF$countyName <- SPDF$NAME_2
    }
    
  }

  # TODO:
  #   # Group polygons with the same identifier
  SPDF <- organizePolygons(SPDF, uniqueID='OBJECTID')
  
  # Assign a name and save the data
  assign(datasetName,SPDF)
  save(list=c(datasetName),file=paste0(dataDir,"/",datasetName,'.RData'))
    
  return(invisible(datasetName))
}

