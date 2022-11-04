# testHarmonization.R

# All spatial data sets in this package should have similar features.  They are:
# Every spatial dataset must contain the following columns:
#   * polygonID – unique identifier for each polygon
#   * countryCode – country at centroid of polygon (ISO 3166-1 alpha-2)
# 
# Spatial datasets at scales smaller than the nation-state should contain the 
# following column(s):
#   * stateCode – ‘state’ at centroid of polygon (ISO 3166-2 alpha-2)
# 
#   All spatial data sets should be in the same CRS:
#   * sp::CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
#
# USAGE:
# library(MazamaSpatialUtils)
# setSpatialDataDir("~/Data/Spatial")
# testHarmonization("HIFLDFederalLands")
#
# TODO: Add function param to specify whether data is expected to be at state level

testHarmonization <- function(
  
  dataset_name

  ) {
  
  # ---- Setup -----------------------------------------------------------------
  MazamaCoreUtils::stopIfNull(getSpatialDataDir())
  MazamaCoreUtils::stopIfNull(dataset_name)
  
  # Open the dataset and return just the base name
  SFDF_name <- loadSpatialData(dataset_name)[1]
  SFDF_name <- stringr::str_split(SFDF_name, "_", simplify = TRUE)[,1]
  SFDF <- get(SFDF_name)
  
  # Create a lits to store messages
  messages <- list()
  
  # ---- Check that all of the mandatory columns are present -------------------
  mandatory_columns <- c("polygonID", "stateCode", "countryCode")
  
  for (col_name in mandatory_columns) {
    if ( col_name %in% colnames(SFDF) == FALSE) {
      messages[length(messages) + 1] <- sprintf("- %s is missing the '%s' column\n", 
                                                dataset_name, col_name)
    }
  }
  
  # ---- Check that polygonID is unique ----------------------------------------
  if ( any(duplicated(SFDF$polygonID)) == TRUE) {
    messages[length(messages) + 1] <- sprintf("- '%s.%s' column contains duplicate values\n", 
                    dataset_name, "polygonID")
  }
  
  # ---- Test that the projection is correct -----------------------------------
  master_proj <-sp::CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
  if (raster::compareCRS(SFDF@proj4string, master_proj) == FALSE) {
    messages[length(messages) + 1] <- sprintf("- '%s' spatial CRS is incorrect: \n%s\n",
                                              dataset_name, SFDF@proj4string)
  }
  
  
  # ---- Print out all the messges at once -------------------------------------
  cat(unlist(messages))
  
}
