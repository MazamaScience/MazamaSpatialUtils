# 6 days of Purple Air Sensor data in the BayArea - 10/24/2019 - 10/29/2019

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

# ----- Get sensor data for this timespan --------------------------------------

library(AirSensor)
library(MazamaSpatialUtils)
library(wesanderson)
library(tibble)

setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")

map_dates <- c("20191024", 
               "20191025", 
               "20191026", 
               "20191027", 
               "20191028", 
               "20191029")

# Create Empty tibble to store all the data
airSensorData <- tibble::tibble(
  ID = character(),
  label = character(),
  longitude = double(),
  latitude = double(),
  pm25_1day = double(),
  date = character()
)

for (my_date in map_dates) {
  air_data <- pas_load(my_date, timezone = "America/Los_Angeles") %>%
    pas_filter(DEVICE_LOCATIONTYPE == "outside") %>%
    pas_filter(stateCode == "CA") %>%
    dplyr::select(longitude, latitude, pm25_1day) %>%
    # NOTE:  summarizeByPolygon doesn't remove NA so we do it here
    dplyr::filter(!is.na(pm25_1day))
  air_data$date <- my_date # add a date column
  head(air_data)
  
  # Combine the day's data into the larger container
  airSensorData <- rbind(airSensorData, air_data)
}


# ---- Load Census tracts geographic boundaries --------------------------------
setwd("local_roger/Bay_Area_Air_Kincade")
load("ca_census_tracts.RData")

# Dump all the fields we don't need
ca_census_tracts@data <- subset(ca_census_tracts@data, 
                                select = c("STATEFP", "COUNTYFP", "TRACTCE"))


# ---- GIS Stuff ---------------------------------------------------------------

# Define our region or interest
sf_region <- bbox <- as(raster::extent(-123.08, -121.65, 37.167, 38.7), 
                        "SpatialPolygons")

# Need to match standard projection
proj <- CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")
proj4string(sf_region) <- proj

# Clip tracts to sf_region
sf_tracts <- simpleClip(ca_census_tracts, sf_region)

# Dissolve tracts so we just have a landmass to plot points on
land <- dissolve(sf_tracts, "STATEFP")

# Convert the Air data into a Spatial Data Frame
airSensorData <- as.data.frame(airSensorData) # Will NOT convert as a tibble
coordinates(airSensorData) <- ~longitude+latitude
proj4string(airSensorData) <- proj

# Clip the Air Data
sf_air <- simpleClip(airSensorData, sf_region)

# ---- Plot Air points using Jon's colors -------------------------------------#

# Use a fun palette with 9 levels (10 breaks)
PAL <- wes_palette("Zissou1", 10, type = "continuous")
BREAKS <- c(0, 5, 10, 15, 20, 25, 50, 100, 250, 2000)

# Use the same intervals to generate a new vector colors
binCode <- .bincode(sf_air$pm25_1day, BREAKS)
cols_sensor <- PAL[binCode]

# Plot land and sensor points
raster::plot(land, col = "gray90", border = "transparent")
plot(sf_region, add = TRUE)

points(
  x = sf_air$longitude,
  y = sf_air$latitude,
  pch = 16,
  cex = 0.5,
  col = cols_sensor
)

legend(
  "topright", 
  legend = paste0(round(BREAKS[1:9])," - ", round(BREAKS[2:10])), 
  fill = PAL
)

title("PurpleAir Sensors, Oct 24 - Oct 29")

# ---- Aggregate Air data by census tracts -------------------------------------

# Step 1. Create a census tract id vector for each air_point
point_tract_codes <- sp::over(sf_air, sf_tracts[,"TRACTCE"])
head(point_tract_codes)

# Step 2. Attach the tract ids back to the air_points
sf_air@data$TRACTCE <- point_tract_codes$TRACTCE
head(sf_air@data)

#   pm25_1day     date TRACTCE
# 1      3.78 20191024  506702
# 2      5.81 20191024  026100
# 3      5.28 20191024  026100
# 4     15.83 20191024  026100
# 5      5.84 20191024  026100
# 6      7.29 20191024  026100

# Step 3. Aggregate air data by date and tract id and get the median of pm25.
#  At this point, we don't care about the air point locations anymore, so we can
#  just grab the @data.  We're going to use it to attach the air_median data 
#  to the census tract polygons.
sf_air_data <- sf_air@data
sf_air_data$pm25_1day <- as.double(sf_air_data$pm25_1day) # Fallout from clip
sf_air_data$date <- as.character(sf_air_data$date)
sf_tract_air_medians <- aggregate(.~TRACTCE+date, sf_air_data, mean)

# That leaves us data that looks like this:
# head(sf_tract_air_medians)
#   TRACTCE     date    pm25_1day
# 1  010401 20191024      3.73
# 2  010500 20191024      6.27
# 3  010510 20191024      2.46
# 4  010512 20191024      2.56
# 5  010600 20191024      7.39
# 6  010602 20191024      3.23

# Looking at one Census tract, we can see the daily medians for the week
# > subset(sf_tract_air_medians, TRACTCE == '010401')
#      TRACTCE     date    pm25_1day
# 1     010401 20191024      3.73
# 440   010401 20191025      4.71
# 886   010401 20191026      7.04
# 1336  010401 20191027      6.50
# 1786  010401 20191028      4.11
# 2168  010401 20191029      9.46

# ---- Attach the daily pm25 medians to the census tracts ----------------------

# This turns out to be amazingly easy using reshape and a left-join
wide_pm25 <- reshape(sf_tract_air_medians, 
                     direction = "wide", 
                     idvar = "TRACTCE", 
                     timevar = "date")

sf_tract_data <- dplyr::left_join(sf_tracts@data, wide_pm25, by = "TRACTCE") 

# Check that the TRACTCE counts match up
length(sf_tracts@data$TRACTCE) == length(sf_tract_data$TRACTCE)

# Replace the sf_tracts@data with the new one
sf_tracts@data <- sf_tract_data

# This gives us a SPDF with a dataframe that contains entries like this:
sample <- subset(sf_tract_data, TRACTCE %in% c("010401", "332000"))
# dplyr::glimpse(sample)                 
# Observations: 2
# Variables: 8
# $ COUNTYFP           <chr> "013", "113"
# $ TRACTCE            <chr> "332000", "010401"
# $ pm25_1day.20191024 <dbl> NA, 3.73
# $ pm25_1day.20191025 <dbl> NA, 4.71
# $ pm25_1day.20191026 <dbl> NA, 7.04
# $ pm25_1day.20191027 <dbl> NA, 6.5
# $ pm25_1day.20191028 <dbl> NA, 4.11
# $ pm25_1day.20191029 <dbl> NA, 9.46
#
# 2 census tracts, 1 with daily values, one without.

# ---- Plot Using spplot -------------------------------------------------------
land.layer <- list("sp.polygons", land, fill = "gray90", border = "transparent")

spplot(sf_tracts,
       zcol="pm25_1day.20191025", # column to use for gradient
       sp.layout=land.layer,       # layout instructions for labels
       col.regions = PAL,   # palette to use
       cuts = 9,                  # number of numbers to put on legend
       col = "gray90",            # border line color
       main=list(label="Air Quality by Tract 2019-10-25",cex=2,font=1)  # Title
)

# This is not giving me the density that I got the other day when I worked with
# a single day's data (20191029).  Need to investigate whether I'm losing data,
# or what else is causing problem.