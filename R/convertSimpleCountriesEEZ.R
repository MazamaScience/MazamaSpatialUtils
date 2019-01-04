#' @keywords datagen
#' @title Convert (Simple) World Borders Shapefile
#' @param nameOnly logical specifying whether to only return the name without creating the file
#' @param dsnPath directory where EEZCountries .zip file is found
#' @description Returns a SpatialPolygonsDataFrame for a simple world divisions
#' @details A previously downloaded world borders shapefile is converted to a
#' SpatialPolygonsDataFrame with additional columns of data. The resulting file will be saved in
#' the data/ directory. The dataset can be downloaded from 
#' \url{http://www.marineregions.org/download_file.php?name=EEZ_land_union_v2_201410.zip}
#' by answering the questions and clicking "download".  
#'
#' The SimpleCountriesEEZ shapefile is the same as the EEZCountries shapefile. Polygons for coastal countries include 
#' a 200 mile buffer, corresponding to their Exclusive Economic Zones, so this shapefile is especially suited
#' for spatial searches. This is the default dataset used in \code{getCountry()} and \code{getCountryCode()}.
#' Users may wish to use a higher resolution dataset when plotting.
#' @note This is a non-exported function used only for updating the package dataset.
#' @return Name of the dataset being created.
#' @references \url{http://www.marineregions.org/downloads.php}
#' @seealso setSpatialDataDir
#' @seealso getCountry, getCountryCode
#' 
convertSimpleCountriesEEZ <- function(dsnPath = NULL, nameOnly=FALSE) {

  # NOTE:  This function only needs to be run once to update the package SimpleCountriesEEZ
  # NOTE:  dataset. It is included here for reproducibility.
  
  # NOTE:  This function should be run wile working with the package source code.
  # NOTE:  The working directory should be MazamaSpatialUtils/ and the resulting
  # NOTE:  .RData file will be kept in the data/ directory
  
  # Sanity check dsnPath
  if ( is.null(dsnPath) ) stop(paste0('Argument dsnPath must be specified. dsnPath indicates the file path for the .zip file downloaded from http://www.marineregions.org/download_file.php?name=EEZ_land_union_v2_201410.zip'))
  if ( !file.exists(dsnPath) ) stop(paste0('dsnPath="',dsnPath,' not found. dsnPath indicates the file path for the .zip file downloaded from http://www.marineregions.org/download_file.php?name=EEZ_land_union_v2_201410.zip'))
  
  sourceCodeDir <- getwd()
  
  # Use package internal data directory
  dataDir <- getSpatialDataDir()
  
  # Specify the name of the dataset and file being created
  datasetName <- 'SimpleCountriesEEZ'
  
  if (nameOnly) return(datasetName)
  
  # Unzip the downloaded file into a new folder, EEZ
  utils::unzip(dsnPath,exdir=file.path(dataDir,'EEZ'))
  dsnPath <- file.path(dataDir,'EEZ')
  
  # Convert shapefile into SpatialPolygonsDataFrame
  # NOTE:  The 'EEZCountries' directory has been created
  # NOTE:  Simplify the .shp file using Mapshaper prior to converting layer
  SPDF <- convertLayer(dsn=dsnPath,layerName='EEZ_land_v2_201410')
  
  # > names(SPDF)
  # [1] "OBJECTID"   "ISO_3digit" "Country"    "Changes"    "Shape_Leng" "Shape_Area"
  
  # Rationalize naming:
  # * human readable full nouns with descriptive prefixes
  # * generally lowerCamelCase
  # with internal standards:
  # * countryCode (ISO 3166-1 alpha-2)
  # * stateCode (ISO 3166-2 alpha-2)
  # * longitude (decimal degrees E)
  # * latitude (decimal degrees N)
  # * area (m^2)
  usefulColumns <- c('OBJECTID', 'ISO_3digit', 'Country', 'Changes')
  SPDF <- SPDF[,usefulColumns]
  names(SPDF) <- c('objectID', 'ISO3', 'countryName', 'changes')
  
  # Change missing countryCodes to NA
  SPDF$ISO3[SPDF$ISO3 == '-' ] <- NA
  
  # NOTE:  http://conjugateprior.org/2013/01/unicode-in-r-packages-not/
  # Transliterate unicode characters for this package-internal dataset
  SPDF$countryName <- iconv(SPDF$countryName, from="UTF-8", to="ASCII//TRANSLIT")
  
  # > SPDF@data[which(stringr::str_length(SPDF$ISO3) != 3),]
  # objectID  ISO3                    countryName changes     area
  # 65        66    CW                        Curacao    <NA>  2.57573
  # 152      154 MNP++ Northern Marinana Islands-Guam    <NA> 82.66514
  SPDF@data["65","ISO3"] <- "CUW"
  SPDF@data["152","ISO3"] <- "GUM"
  
  # Add more standard columns
  SPDF$countryCode <- iso3ToIso2(SPDF$ISO3)
  SPDF <- organizePolygons(SPDF, uniqueID='objectID')
  
  # TODO:  Fix or remove columns with missing countryCode
  
  #     objectID ISO3                                              countryName changes countryCode polygonID
  # 1          1 <NA>                               Conflict zone Japan/Russia    <NA>        <NA>         1
  # 2          2 <NA>                          Conflict zone Japan/South Korea    <NA>        <NA>         2
  # 3          3 <NA>                                 Joint regime Japan/Korea    <NA>        <NA>         3
  # 4          4 <NA>                         Conflict zone China/Japan/Taiwan    <NA>        <NA>         4
  # 5          5 <NA>                                          Spratly Islands    <NA>        <NA>         5
  # 6          6 <NA>                            Joint regime Colombia/Jamaica    <NA>        <NA>         6
  # 7          7 <NA>               Joint regime Nigeria/Sao Tome and Principe    <NA>        <NA>         7
  # 8          8 <NA>              Joint development area Australia/East Timor    <NA>        <NA>         8
  # 9          9 <NA>                Protected zone Australia/Papua New Guinea    <NA>        <NA>         9
  # 32        32  BES                            Bonaire, Sint-Eustasius, Saba    <NA>        <NA>        32
  # 62        62  CPT                                        Clipperton Island    <NA>        <NA>        62
  # 66        66  CUW                                                  Curacao    <NA>        <NA>        66
  # 211      211  SXM                                             Sint Maarten    <NA>        <NA>       211
  # 245      245 <NA>                                          Paracel Islands    <NA>        <NA>       245
  # 246      246 <NA>                      Area of overlap Australia/Indonesia    <NA>        <NA>       246
  # 250      250 <NA>                                   Disputed Kenya/Somalia 2014-10        <NA>       250
  # 251      251 <NA>                       Disputed Western Sahara/Mauritania 2014-10        <NA>       251
  # 252      252 <NA>                      Disputed Barbados/Trinidad & Tobago 2014-10        <NA>       252
  # 255      255 <NA> Area en controversia (disputed - Peruvian point of view)    <NA>        <NA>       255
  # 260      260  VTC                                             Vatican City 2014-10        <NA>       260
  
  undefinedISO3 <- c("BES", "CPT", "CUW", "SXM", "VTC")
  rnamesUndefinedISO3 <- c("32", "62", "66", "211", "260")
  undefinedISO2 <- c("BQ", "FR", "CW", "SX", "VA")
  SPDF@data[rnamesUndefinedISO3,"countryCode"] <- undefinedISO2
  
  # remove rows with <NA> country codes
  
  notNA <- !is.na(SPDF$countryCode)
  SPDF <- SPDF[notNA,]
  
  # Assign a name and save the data
  assign(datasetName,SPDF)
  save(list=c(datasetName),file=paste0(sourceCodeDir,'/data/',datasetName,'.RData'))
  
  # Clean up
  unlink(dsnPath, recursive=TRUE, force=TRUE)
  
  return(invisible(datasetName))
  
}
