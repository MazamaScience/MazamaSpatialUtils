
# ===== California AirSensor USCensusCounty ====================================

# ----- Set up configurable parameters -----------------------------------------

# Use a fun palette with 9 levels (10 breaks)
library(wesanderson)
PAL <- wes_palette("Zissou1", 9, type = "continuous")
BREAKS <- c(0, 5, 10, 15, 20, 25, 50, 100, 250, 2000)

DATE <- "20191029" # Kincade fire
#DATE <- "20200220"

# ----- Set up spatial data ----------------------------------------------------

library(MazamaSpatialUtils)
setSpatialDataDir("~/Data/Spatial")
loadSpatialData("USCensusCounties")
CA <- subset(USCensusCounties, stateCode == "CA")

# ----- Set up sensor data -----------------------------------------------------

library(AirSensor)
###setArchiveBaseUrl("https://airfire-data-exports.s3-us-west-2.amazonaws.com/PurpleAir/v1")
setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")

ca_sensorData <- 
  pas_load(DATE, timezone = "America/Los_Angeles") %>% # Kincade fire!!!
  ###pas_load(20200220, timezone = "America/Los_Angeles") %>% 
  pas_filter(DEVICE_LOCATIONTYPE == "outside") %>%
  pas_filter(stateCode == "CA") %>%
  dplyr::select(longitude, latitude, pm25_1day) %>%
  # NOTE:  summarizeByPolygon doesn't remove NA so we do it here
  dplyr::filter(!is.na(pm25_1day))
  
# ----- Assign sensor colors ----------------------------------------------------------

# Use the same intervals to generate a new vector colors
binCode <- .bincode(ca_sensorData$pm25_1day, BREAKS)
cols_sensor <- PAL[binCode]

# ----- sp map with colored points ---------------------------------------------

# Map with points
plot(CA)

points(
  x = ca_sensorData$longitude,
  y = ca_sensorData$latitude,
  pch = 16,
  cex = 0.5,
  col = cols_sensor
)

legend(
  "topright", 
  legend = paste0(round(BREAKS[1:9])," - ", round(BREAKS[2:10])), 
  fill = PAL
)

title(sprintf("PurpleAir Sensors on %s", DATE))


# ----- Chloropleth map --------------------------------------------------------

summaryByCounty <- summarizeByPolygon(
  longitude = ca_sensorData$longitude,
  latitude = ca_sensorData$latitude,
  value = ca_sensorData$pm25_1day,
  SFDF = CA,
  useBuffering = FALSE,
  FUN = median,
  varName = "pm25_mean"
)

# Merge this with CA@data using sp::merge() function
CA2 <- merge(CA, summaryByCounty, by = "polygonID")

# Use the same intervals to generate a new vector colors
binCode <- .bincode(CA2$pm25_mean, BREAKS)
cols_county <- PAL[binCode]

# Change NA to "#bbbbbb" so that we get gray instead of white for NA counties
cols_county[is.na(cols_county)] <- "#CCCCCC"

# Chloropleth map
plot(CA2, col = cols_county, border = "gray90", lwd = 0.5)

# Add points with white borders so we can see where they agree/disagree
points(
  x = ca_sensorData$longitude,
  y = ca_sensorData$latitude,
  pch = 21,
  cex = 0.5,
  bg = cols_sensor,
  col = "white",
  lwd = 0.3
)

legend(
  "topright", 
  legend = paste0(round(BREAKS[1:9])," - ", round(BREAKS[2:10])), 
  fill = PAL
)

title(sprintf("County Mean PA sensor value on %s", DATE))


# ===== California AirSensor <other SFDF> ====================================

