#' @export
#' @title Run Shiny app example
#' @param appName app to run
#' @param ... parameters passed on to \code{runApp()}. 
#' @description This function will run the specified shiny app. By default, the 
#' app will open in a new window. By default, it will run in the foreground in 
#' your R console, meaning that you have to stop the app to use R again. The 
#' default app is "map_app" which requires that the WBDHUC datasets and 
#' \code{NaturalEarthAdm1} be downloaded to 
#' SpatialDataDir. They can be installed with \code{\link{convertWBDHUC}}. 

runExample <- function(
  appName = "map_app", 
  ...
) {
  
  appDir <- system.file("shiny_examples", appName, package = "MazamaSpatialUtils")
  
  if ( appDir == "" ) {
    stop("Could not find example directory.",
         "Try re-installing \"MazamaSpatialUtils\".", 
         call. = FALSE)
  }
  
  shiny::runApp(appDir, ...)
  
}