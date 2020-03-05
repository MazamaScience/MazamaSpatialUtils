library(dplyr)
library(raster)
library(sp)
library(AirSensor)
setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")


# Setup plot colors and bin sizes
library(wesanderson)
PAL <- wes_palette("Zissou1", 9, type = "continuous")
BREAKS <- c(0, 5, 10, 15, 20, 25, 50, 100, 250, 2000)

# Work with extents to get a sweet spot of coverage.
bbox <- as(raster::extent(-123.75, -119.45, 36.5, 39.5), "SpatialPolygons") #tweaked MazSatUtils Kincade extent

# Load 2 data sets in to evaluate coverage
# load("calif_counties.RData")
load("ca_census_tracts.RData")

oct_29_air <- pas_load("20191029", timezone = "America/Los_Angeles") %>%
    pas_filter(DEVICE_LOCATIONTYPE == "outside") %>%
    pas_filter(stateCode == "CA") %>%
    dplyr::select(longitude, latitude, pm25_1day) %>%
    # NOTE:  summarizeByPolygon doesn't remove NA so we do it here
    dplyr::filter(!is.na(pm25_1day))

# Basic plot for EDA
plot(bbox)
plot(ca_census_tracts, col = "gray90", border = "gray90", add = TRUE)
plot(bbox, add = TRUE)

# Probabaly could shift East slightly, but let's look at the point coverage first.

# Use the same intervals to generate a new vector colors
binCode <- .bincode(oct_29_air$pm25_1day, BREAKS)
cols_sensor <- PAL[binCode]

points(
  x = oct_29_air$longitude,
  y = oct_29_air$latitude,
  pch = 16,
  cex = 0.5,
  col = cols_sensor
)

# Looks to me like there is a very high coverage in the areas where there are cities.

# Define a smaller sf_region to use with census tracts
sf_region <- bbox <- as(raster::extent(-123.08, -121.65, 37.167, 38.7), "SpatialPolygons")
plot(sf_region, add = TRUE)

# Going to start with that as my initial clipping extent

# Clip census tracts and counties to sf_region

simpleClip <- function(
    # Returns the intersection of spdf_1 and spdf 2
    # but only returns attributes for spdf_1
    spdf_1,
    spdf_2
) {
    clipped <- raster::intersect(spdf_1, spdf_2)
    keepN <- ncol(spdf_1@data) # Only keep the 1st N cols that belong to spdf_1
    data <- clipped@data[1:keepN]
    names(data) <- sub("\\.1", "", colnames(data))
    clipped@data <- data
    return(clipped)
}

# Set clip region to common projection
proj <- CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")
proj4string(sf_region) <- proj

# Clip the tracts to the extents of the sf_region
sf_region_tracts <- simpleClip(ca_census_tracts, sf_region)

# Plot the results
plot(sf_region_tracts, col = "gray90", border = "gray", lwd = .1)
plot(sf_region, add = TRUE)

points(
  x = oct_29_air$longitude,
  y = oct_29_air$latitude,
  pch = 16,
  cex = 0.5,
  col = cols_sensor
)

# Convert air_data into SpatialPointsDataFrame
oct_29_air_df <- data.frame(pm25_1day = oct_29_air$pm25_1day,
                            longitude = oct_29_air$longitude,
                            latitude = oct_29_air$latitude)

xy <- oct_29_air_df[c("longitude", "latitude")]

oct_29_air_spdf <- SpatialPointsDataFrame(coords = xy, data = oct_29_air_df, proj4string = proj)

# Spatially subset the points to just those inside the sf_region
sf_region_oct_29_air <- simpleClip(oct_29_air_spdf, sf_region)

plot(sf_region_tracts, col = "gray90", border = "gray", lwd = .15)
plot(sf_region_oct_29_air,
     pch = 16,
     cex = .85,
     col = cols_sensor,
     add = TRUE)
plot(sf_region, add = TRUE)

# Aggregate up the census tracts into counties and see what they look like over tracts
sf_region_counties <- raster::aggregate(sf_region_tracts,
                                        by='COUNTYFP',
                                        sums=list(list(function(x)x[1], c('STATEFP')  )))
# Better                                                      
plot(sf_region)
plot(sf_region_tracts, add = TRUE)
plot(sf_region_counties, border = "blue", lwd = 2, add = TRUE)

# Color the census tracts using the same colors. This means that for every point, I want to assign it a
# census_tract_id.  Easiest way to do that is to intersect the tracts with the points.

point_tract_codes <- sp::over(sf_region_oct_29_air, sf_region_tracts[,"TRACTCE"])

sf_region_oct_29_air@data$TRACTCE <- point_tract_codes$TRACTCE
head(sf_region_oct_29_air@data)

# Now what I want is to aggregate this data by TRACTCE, taking the median of all pm25_1day values for every
# tract

oct_29_air_median_by_tract <- aggregate(sf_region_oct_29_air$pm25_1day,
                                        list(sf_region_oct_29_air$TRACTCE),
                                        median)
names(oct_29_air_median_by_tract) = c('TRACTCE', 'oct_29_pm25')
head(oct_29_air_median_by_tract)

#  Now I need to tie the 'oct_29_pm25' values back to every 'TRACTCE' in 
# 'sf_region_oct_29_air' that matches
sf_region_tracts@data <- merge(sf_region_tracts@data, 
                               oct_29_air_median_by_tract, 
                               by = 'TRACTCE')

subset(sf_region_tracts@data, sf_region_tracts@data$TRACTCE == '026100')

# Now to plot the census tracts using the aggregated pm25 values
# Use the same intervals to generate a new vector colors
binCode <- .bincode(sf_region_tracts$oct_29_pm25, BREAKS)
cols_tract <- PAL[binCode]

# Change NA to "#bbbbbb" so that we get gray instead of white for NA counties
cols_tract[is.na(cols_tract)] <- "#CCCCCC"

# Chloropleth map
plot(sf_region_tracts, col = cols_tract, border = "transparent", lwd = 0.5)
plot(sf_region, add = TRUE, lwd = 1.5)
title("Original Oct 29. 2019 Map")
