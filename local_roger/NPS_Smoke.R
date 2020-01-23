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
# [X] 2. Get all of the National Parks
# [ ] 3. Determine what parks have a monitor station in them (or nearby?)
#   - [X] Are there in fact any parks with monitors?
#   - [X] How many of the total parks have a station?
#   - [X] Is this a meaningful data set to look at? (no, not enough total coverage)
# [X] 4. Isolate the in_park stations
# [X] 5. Pull the in-park monitor data for the months of Jun - Sep 2018
# [ ] 6. Figure out which parks had alot of smoke
# [ ] 7. Figure out which park was the worst
# 
# May need to pick a specific park that has monitors, like Glacier, and analyze
# by time instead.

library(PWFSLSmoke)
library(MazamaSpatialUtils)
library(rmapshaper)
library(raster)

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

# See lots of data points in Canada that I don't want.  Going to remove them by
# using the countryCode in the the monitor data
conus_monitor_spdf <- subset(monitor_spdf, monitor_spdf@data$countryCode == 'US')
plot(conus_monitor_spdf, col = "forestgreen", add = TRUE)

# Also see that these state borders are too detailed. Simplifying to 1%
conus_states_01 <- rmapshaper::ms_simplify(conus_states, 0.01)
conus_states_01@data$rmapshaperid <- NULL

# ---- Get the National Parks out of the HIFLDFederalLands data ----------------
loadSpatialData("HIFLDFederalLands")
nps_lands <- subset(HIFLDFederalLands, HIFLDFederalLands@data$agencyCode %in% c("NPS", "FS"))

# Quick and dirty plot of what we have so far
dev.off()
plot(conus_states_01, col = "grey90")
plot(nps_lands, col = "goldenrod", add = TRUE)
plot(conus_monitor_spdf, col = "forestgreen", add = TRUE)

#reset the projections for all of our spatial data to 
# USA_Contiguous_Lambert_Conformal_Conic, esri:102004

lcc_conus_states <- spTransform(conus_states_01, CRS("+init=esri:102004"))
lcc_nps_lands <- spTransform(nps_lands, CRS("+init=esri:102004"))
lcc_monitor <- spTransform(conus_monitor_spdf, CRS("+init=esri:102004") )

# Let's seee if there are any monitors that intersect with NPS lands
lcc_nps_monitor <- lcc_monitor[lcc_nps_lands,]

# Replotting
dev.off()
plot(lcc_conus_states, col = "grey90")
plot(lcc_nps_lands, col = "goldenrod", add = TRUE)
plot(lcc_monitor, col = "forestgreen", add = TRUE)
plot(lcc_nps_monitor, col = "red", add = TRUE)

# There are some, but many of the larger parks, like Olympic in WA are missing
#  How many monitors are there inside parks?  Ans = 39
length(lcc_nps_monitor)

# Going to have to think about this a bit.  I could do a buffered search and tie 
# nearby monitor locations to parks that don't have any.  Or I could switch to 
# using a different feature type from the Fed Lands data.

# Let's get the names of all the parks for which there are monitors.  Since 
# there may be more than one monitor in a park, I'm going to attach the park 
# name from federal lands to the lcc_nps_monitor points as a new column.  This 
# appears to be most easily done using the raster package, but I'm going to also 
# test the rgeos::gIntersection 

# raster::instersect
# intersect(x, y)
# if x is a SpatialPoints* object: SpatialPoints*

joined_lcc_nps_monitor <- raster::intersect(lcc_nps_monitor, lcc_nps_lands)

joined_lcc_nps_monitor <- subset(joined_lcc_nps_monitor, 
                                 select = c("monitorID", "longitude", "latitude", "stateCode", "countryCode", "primaryLandType", "agencyCode", "primaryName", "stateCode" ))

# So how are are the monitors distributed?
print(dplyr::select(joined_lcc_nps_monitor@data, primaryName, stateCode) %>% group_by(primaryName, stateCode) %>% summarise(n()))

# 1 Acadia National Park                               1
# 2 Badlands National Park                             1
# 3 Bryce Canyon National Park                         1
# 4 Crater Lake National Park                          1
# 5 Glacier National Park                              2
# 6 Indiana Dunes National Lakeshore                   1
# 7 Joshua Tree National Park                          1
# 8 Kaloko-Honokohau National Historical Park          3
# 9 Kings Canyon National Park                         4
# 10 Mississippi National River And Recreation Area     1
# 11 Padre Island National Seashore                     1
# 12 Sequoia National Park                              9
# 13 Shenandoah National Park                           1
# 14 Theodore Roosevelt National Park                   1
# 15 Wind Cave National Park                            1
# 16 Yellowstone National Park                          1
# 17 Yosemite National Park                             7
# 18 Yosemite Wilderness                                2