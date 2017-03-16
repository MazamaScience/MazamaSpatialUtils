#' @keywords environment
#' @export
#' @title Load Spatial Datasets
#' @param pattern regular expression used to match filenames
#' @description Load datasets found in the directory previously set with \code{setSpatialDataDir()}.
#' Only files matching \code{pattern} will be loaded.
#' 
#' Core datastes available for the package include:
#' \itemize{
#' \item{\code{TMWorldBorders} -- high resolution country polygons (higher resolution than \code{SimpleCountries})}
#' \item{\code{NaturalEarthAdm1} -- state/province polygons throughout the world}
#' \item{\code{USCensusCounties} -- county polygons in the United States}
#' \item{\code{WorldTimezones} -- high resolution timezone polygons (higher resolution than \code{SimpleTimezones})}
#' }
#' 
#' These can be installed with \code{installSpatialData()}.
#' @return Invisibly returns a vector of spatial dataset names loaded into the global environment.
#' @seealso setSpatialDataDir
#' @seealso installSpatialData
loadSpatialData <- function(pattern='*') {
  
  # Use package internal data directory
  dataDir <- getSpatialDataDir()
  
  filePaths <- list.files(dataDir, pattern, full.names=TRUE)
  
  if ( length(filePaths) == 0 ) {
    
    stop(paste0('No files matching "',pattern,'" found in ',dataDir,'.'), call.=FALSE)
    
  } else {
    
    for ( filePath in filePaths ) {
      load(filePath, envir=.GlobalEnv)
    }
    
    # Return names of all SPDF laoded into the global environment
    names <- base::basename(filePaths)
    names <- stringr::str_replace(names, '\\.RData', '')
    return(invisible(names))
    
  }
  
}

