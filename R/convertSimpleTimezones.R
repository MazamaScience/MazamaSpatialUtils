#' @keywords datagen
#' @title Convert SimpleTimezones Shapefile
#' @param nameOnly logical specifying whether to only return the name without creating the file
#' @description A world timezone shapefile is downloaded from \url{http://efele.net/maps/tz/world/}
#' and converted to a SpatialPolygonsDataFrame with additional columns of data. The resulting file will be created
#' in the spatial data directory which is set with \code{setSpatialDataDir()}.
#' @note
#' The following list of timezones have polygons but the associated rows in the dataframe have no data.
#' These timezones also have no \code{countryCode} assigned. We hope to rectify this in a future release.
#' \preformatted{
#' > WorldTimezones@data$timezone[is.na(WorldTimezones$countryCode)]
#' [1] "Europe/Zagreb"         "Europe/Vatican"        "America/Coral_Harbour"
#' [4] "Arctic/Longyearbyen"   "uninhabited"           "America/Kralendijk"
#' [7] "Europe/Jersey"         "Europe/Bratislava"     "America/St_Barthelemy"
#' [10] "Europe/Ljubljana"      "Europe/Mariehamn"      "Europe/Podgorica"
#' [13] "Europe/Isle_of_Man"    "Europe/Guernsey"       "Europe/San_Marino"
#' [16] "Europe/Skopje"         "Europe/Sarajevo"       "America/Lower_Princes"
#' [19] "America/Marigot"       "Africa/Juba"
#' }
#' @note This is a non-exported function used only for updating the package dataset.
#' @return Name of the dataset being created.
#' @references \url{http://efele.net/maps/tz/world/}
#' @seealso setSpatialDataDir
#' @seealso convertWikipediaTimezoneTable
#' @examples
#' \dontrun{
#' setSpatialDataDir(getwd()) # directory
#' convertSimpleCountries()
#' }
convertSimpleTimezones <- function(nameOnly=FALSE) {

  # NOTE:  This function only needs to be run once to update the package SimpleTimezones
  # NOTE:  dataset. It is included here for reproducibility.

  # NOTE:  This function should be run wile working with the package source code.
  # NOTE:  The working directory should be MazamaSpatialUtils/ and the resulting
  # NOTE:  .RData file will be kept in the data/ directory
  
  sourceCodeDir <- getwd()
  
  # Use package internal data directory
  dataDir <- getSpatialDataDir()

  # Specify the name of the file being created
  datasetName <- 'SimpleTimezones'

  if (nameOnly) return(datasetName)

  # Build appropriate request URL for world timezones
  url <- "http://efele.net/maps/tz/world/tz_world.zip"

  filePath <- paste(dataDir,basename(url),sep='/')
  utils::download.file(url,filePath)
  utils::unzip(filePath,exdir=dataDir)
  
  # Convert shapefile into SpatialPolygonsDataFrame
  dsnPath <- paste(dataDir,'world',sep='/')
  SPDF <- convertLayer(dsn=dsnPath,layerName='tz_world')

  # Rename "TZID" to "timezone"
  names(SPDF@data) <- c('timezone')

  # Now get additional data from Wikipedia
  wikipediaTimezoneTable <- convertWikipediaTimezoneTable()

  # Merge the additional data onto the @data slot of the SPDF
  SPDF@data <- dplyr::left_join(SPDF@data, wikipediaTimezoneTable, by="timezone")

  # Group polygons with the same identifier
  SPDF <- organizePolygons(SPDF, uniqueID='timezone')

  # Simplify to 5% of the vertices
  SPDF <- rmapshaper::ms_simplify(SPDF, 0.05)
  SPDF@data$rmapshaperid <- NULL
  
  # Assign a name and save the data
  assign(datasetName,SPDF)
  save(list=c(datasetName),file=paste0(sourceCodeDir,'/data/',datasetName,'.RData'))

  # Clean up
  unlink(filePath, force=TRUE)
  unlink(dsnPath, recursive=TRUE, force=TRUE)
  
  return(invisible(datasetName))
  
}

