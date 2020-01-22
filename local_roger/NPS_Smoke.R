# PWFSLSmoke + MazamaSpatialUtils example
# =======================================
#   
#   The Summer of 2018 saw another intense wildfire season in the U.S. (West?).
# By combining information from the PWFSLSmoke monitoring package and the new
# HIFLDFederalLands data set in MazamaSpatialUtils, we can see which National 
# Parks were most affected my dense smoke and unhealthy PM 2.5 levels.
# 
# Plan of attack
# --------------
# [X] 1. Get all of the monitor locations
# [ ] 2. Get all of the National Parks
# [ ] 3. Determine what parks have a monitor station in them (or nearby?)
#   - [ ] Are there in fact any parks with monitors?
#   - [ ] How many of the total parks have a station?
#   - [ ] Is this a meaningful data set to look at?
# [ ] 4. Isolate the in_park stations
# [X] 5. Pull the in-park monitor data for the months of Jun - Sep 2018
# [ ] 6. Figure out which parks had alot of smoke
# [ ] 7. Figure out which park was the worst

library(PWFSLSmoke)
library(MazamaSpatialUtils)

setSpatialDataDir("~/Data/Spatial")
loadSpatialData("HIFLDFederalLands")

# ---- Get all of the monitor locations ----------------------------------------
us_monitors <- monitor_loadAnnual(2018) %>% monitor_subset(tlim = c(20180601,20181030))

# ---- Load US States and plot the monitor locations over it. ------------------
loadSpatialData("USCensusStates")

# create a CONUS subset that excludes Alaska, Hawaii, etc.
omit_codes <- c("HI", "AK", "AS", "PR", "GU", "MP", "VI")
conus_states <- subset(USCensusStates, !(USCensusStates@data$stateCode %in% omit_codes))
plot(conus_states)

# Get the monitor locations into a SpatialPointsDataFrame. This will allow us to 
# use rgeos and do some spatial subsetting

monitor_location_df <- subset(us_monitors$meta, 
                              select = c("monitorID", "longitude", "latitude", "stateCode", "countryCode"))

xy <- monitor_location_df[c("longitude", "latitude")]

monitor_spdf <- SpatialPointsDataFrame(coords = xy, data = monitor_location_df,
                                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# quick and dirty plot of CONUS with monitor locations on it
plot(conus_states)
plot(monitor_spdf, col = "red", add = TRUE)
