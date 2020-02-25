# Bay Area Air Quality - 24 Oct 2019 - 29 Oct 2019

# Purple Air sensor data by day
library(AirSensor)
setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")

oct_24_air <- 
  pas_load(20191024, timezone = "America/Los_Angeles") %>% 
  pas_filter(DEVICE_LOCATIONTYPE == "outside") %>%
  pas_filter(stateCode == "CA") %>%
  dplyr::select(longitude, latitude, pm25_1day) %>%
  # NOTE:  summarizeByPolygon doesn't remove NA so we do it here
  dplyr::filter(!is.na(pm25_1day))

oct_25_air <- 
  pas_load(20191025, timezone = "America/Los_Angeles") %>% 
  pas_filter(DEVICE_LOCATIONTYPE == "outside") %>%
  pas_filter(stateCode == "CA") %>%
  dplyr::select(longitude, latitude, pm25_1day) %>%
  # NOTE:  summarizeByPolygon doesn't remove NA so we do it here
  dplyr::filter(!is.na(pm25_1day))

oct_26_air <- 
  pas_load(20191026, timezone = "America/Los_Angeles") %>% 
  pas_filter(DEVICE_LOCATIONTYPE == "outside") %>%
  pas_filter(stateCode == "CA") %>%
  dplyr::select(longitude, latitude, pm25_1day) %>%
  # NOTE:  summarizeByPolygon doesn't remove NA so we do it here
  dplyr::filter(!is.na(pm25_1day))

oct_27_air <- 
  pas_load(20191027, timezone = "America/Los_Angeles") %>% 
  pas_filter(DEVICE_LOCATIONTYPE == "outside") %>%
  pas_filter(stateCode == "CA") %>%
  dplyr::select(longitude, latitude, pm25_1day) %>%
  # NOTE:  summarizeByPolygon doesn't remove NA so we do it here
  dplyr::filter(!is.na(pm25_1day))

oct_28_air <- 
  pas_load(20191028, timezone = "America/Los_Angeles") %>% 
  pas_filter(DEVICE_LOCATIONTYPE == "outside") %>%
  pas_filter(stateCode == "CA") %>%
  dplyr::select(longitude, latitude, pm25_1day) %>%
  # NOTE:  summarizeByPolygon doesn't remove NA so we do it here
  dplyr::filter(!is.na(pm25_1day))

oct_29_air <- 
  pas_load(20191029, timezone = "America/Los_Angeles") %>% 
  pas_filter(DEVICE_LOCATIONTYPE == "outside") %>%
  pas_filter(stateCode == "CA") %>%
  dplyr::select(longitude, latitude, pm25_1day) %>%
  # NOTE:  summarizeByPolygon doesn't remove NA so we do it here
  dplyr::filter(!is.na(pm25_1day))

# Save off the data so we can use it later
assign("oct_24_air", oct_24_air)
save(list = c("oct_24_air"), file = 'oct_24_air.RData')

assign("oct_25_air", oct_25_air)
save(list = c("oct_25_air"), file = 'oct_25_air.RData')

assign("oct_26_air", oct_26_air)
save(list = c("oct_26_air"), file = 'oct_26_air.RData')

assign("oct_27_air", oct_27_air)
save(list = c("oct_27_air"), file = 'oct_27_air.RData')

assign("oct_28_air", oct_28_air)
save(list = c("oct_28_air"), file = 'oct_28_air.RData')

assign("oct_29_air", oct_29_air)
save(list = c("oct_29_air"), file = 'oct_29_air.RData')

# County borders for CA
library(MazamaSpatialUtils)
setSpatialDataDir("~/Data/Spatial")
loadSpatialData("USCensusCounties")
CA <- subset(USCensusCounties, stateCode == "CA")

assign("calif_counties", CA)
save(list = c("calif_counties"), file = 'calif_counties.RData')