# PWFSLSmoke + MazamaSpatialUtils example
# =======================================
#   
#   The Summer of 2018 saw another intense wildfire season in the U.S. West.
# By combining information from the PWFSLSmoke monitoring package and the new
# HIFLDFederalLands data set in MazamaSpatialUtils, we can see what Federally 
# administered Parks and Forests were most affected my dense smoke and 
# unhealthy PM 2.5 levels.
# 
# Plan of attack
# --------------
# [X] 1. Get all of the monitor locations
# [X] 2. Get all of the National Parks and Forests
# [X] 3. Determine what parks (& forests) have a monitor station in them
# [X] 4. Isolate the in-park stations
# [X] 5. Pull the in-park monitor data for the months of Jun - Sep 2018
# [ ] 6. Figure out which parks had alot of smoke that summer
# [ ] 7. Figure out which parks were the worst

library(PWFSLSmoke)
library(MazamaSpatialUtils)
library(rmapshaper)
library(raster)

setSpatialDataDir("~/Data/Spatial")
loadSpatialData("HIFLDFederalLands")

# ---- Get all of the monitor locations ----------------------------------------
west_codes <- c("WA", "ID", "MT", "WY", "CO", "UT", "NM", "AZ", "NV", "CA", "OR")
west_monitors <- monitor_loadAnnual(2018) %>% 
  monitor_subset(stateCodes = west_codes) %>%
  monitor_subset(tlim = c(20180601,20181030))

# ---- Load US States and plot the monitor locations over it. ------------------
loadSpatialData("USCensusStates")

# create a subset of Western states 
west_states <- subset(USCensusStates, (USCensusStates@data$stateCode %in% 
                                         west_codes))
plot(west_states)

# Get the monitor locations into a SpatialPointsDataFrame. This will allow us to 
# use rgeos and do some spatial subsetting

monitor_location_df <- subset(west_monitors$meta, 
                              select = c("monitorID", "longitude", "latitude", "stateCode", "countryCode"))

xy <- monitor_location_df[c("longitude", "latitude")]

monitor_spdf <- SpatialPointsDataFrame(coords = xy, data = monitor_location_df,
                                       proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# quick and dirty plot of CONUS with monitor locations on it
plot(west_states)
plot(monitor_spdf, col = "red", add = TRUE)

# Also see that these state borders are too detailed. Simplifying to 1%
west_states_01 <- rmapshaper::ms_simplify(west_states, 0.01)
west_states_01@data$rmapshaperid <- NULL

# ---- Get the National Parks out of the HIFLDFederalLands data ----------------
loadSpatialData("HIFLDFederalLands")
fed_lands <- subset(HIFLDFederalLands, HIFLDFederalLands@data$agencyCode %in% 
                      c("NPS", "FS"))
# Select only the western states
fed_lands <- subset(fed_lands, (fed_lands@data$stateCode %in% 
                                         west_codes))


# Quick and dirty plot of what we have so far
dev.off()
plot(west_states_01, col = "grey90", border = "white")
plot(fed_lands, col = "goldenrod", border = "transparent", add = TRUE)
plot(monitor_spdf, col = "forestgreen", add = TRUE)

#reset the projections for all of our spatial data
merc_west_states <- spTransform(west_states_01, CRS("+init=epsg:3857"))
merc_fed_lands <- spTransform(fed_lands, CRS("+init=epsg:3857"))
merc_monitor <- spTransform(monitor_spdf, CRS("+init=epsg:3857") )

# Let's seee if there are any monitors that intersect with NPS lands
merc_fed_monitor <- merc_monitor[merc_nps_lands,]

# Replotting to show everything
dev.off()
plot(merc_west_states, col = "grey90", border = "white")

# Color fed_ands by agency
agency_idx <- as.factor(c("NPS", "FS"))
color_list <- c("forestgreen", "goldenrod")
plot(merc_fed_lands, col = color_list[agency_idx], add = TRUE, border = "transparent")
# plot(merc_monitor, col = "forestgreen", add = TRUE)
plot(merc_west_states, col = "transparent", border = "white", add = TRUE)
plot(merc_fed_monitor, add = TRUE)
# Add a title
title("Monitors in Federal Lands of the West", cex = 1.8)
# Add a legend that matched color to agency
legend("bottomleft", fill = color_list, legend = as.character(unique(agency_idx)))


#  How many monitors are there inside parks/forests?  Ans = 125
length(merc_fed_monitor)

# Let's get the names of all the parks for which there are monitors.  Since 
# there may be more than one monitor in a park, I'm going to attach the park 
# name from federal lands to the lcc_nps_monitor points as a new column.  This 
# appears to be most easily done using the raster package:
#     raster::instersect
#     intersect(x, y)
#     if x is a SpatialPoints* object, returns SpatialPoints*

joined_merc_fed_monitor <- raster::intersect(merc_fed_monitor, merc_fed_lands)

joined_merc_fed_monitor <- subset(joined_merc_fed_monitor, 
                                 select = c("monitorID", "longitude", "latitude", "stateCode", "countryCode", "primaryLandType", "agencyCode", "primaryName", "stateCode" ))

# So how are are the monitors distributed?
View(dplyr::select(joined_merc_fed_monitor@data, primaryName, stateCode) %>% 
        group_by(primaryName, stateCode) %>% summarise(n()))
