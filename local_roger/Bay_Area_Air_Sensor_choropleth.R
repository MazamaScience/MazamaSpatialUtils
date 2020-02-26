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
    dplyr::select(ID, label, longitude, latitude, pm25_1day) %>%
    # NOTE:  summarizeByPolygon doesn't remove NA so we do it here
    dplyr::filter(!is.na(pm25_1day))
  air_data$date <- my_date # add a date column
  head(air_data)
  
  # Combine the day's data into the larger container
  airSensorData <- rbind(airSensorData, air_data)
}


# ---- Load Census tracts geographic boundaries --------------------------------
#setwd("local_roger/Bay_Area_Air_Kincade")
load("ca_census_tracts.RData")


# ---- Clip census tracts data to SF Region ------------------------------------

# Define our region or interest
sf_region <- bbox <- as(raster::extent(-123.08, -121.65, 37.167, 38.7), 
                        "SpatialPolygons")

# Need to match standard projection
proj <- CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")
proj4string(sf_region) <- proj

# Clip tracts to sf_region
sf_tracts <- simpleClip(ca_census_tracts, sf_region)

# Dissolve tracts so we just have a landmass to plot points on
land <- dissolve(sf_tracts, "STATEFP", border = "transparent")

# Convert the Air data into a Spatial Data Frame
airSensorData <- as.data.frame(airSensorData) # Will NOT convert as a tibble
coordinates(airSensorData) <- ~longitude+latitude
proj4string(airSensorData) <- proj

# ---- Plot Air points using Jon's colors -------------------------------------#

# Use a fun palette with 9 levels (10 breaks)
PAL <- wes_palette("Zissou1", 9, type = "continuous")
BREAKS <- c(0, 5, 10, 15, 20, 25, 50, 100, 250, 2000)

# Use the same intervals to generate a new vector colors
binCode <- .bincode(airSensorData$pm25_1day, BREAKS)
cols_sensor <- PAL[binCode]

# Plot land and sensor points
raster::plot(land, col = "gray90", border = "transparent")
plot(sf_region, add = TRUE)

points(
  x = airSensorData$longitude,
  y = airSensorData$latitude,
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


