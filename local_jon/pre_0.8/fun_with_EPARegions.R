# Testing the USCensusCBSA dataset

library(dplyr)
library(MazamaSpatialUtils)
setSpatialDataDir("~/Data/Spatial")

loadSpatialData("EPARegions")

# What region is Washington in?
EPARegions@data %>%
  filter(stringr::str_detect(allStateCodes, "WA")) %>%
  select(epaRegion)

# Good

# Let's grab some monitoring data and extract the associated EPARegion

library(PWFSLSmoke)

meta <- 
  monitor_loadLatest() %>%
  monitor_subset(stateCodes = c("AZ", "NM")) %>%
  monitor_extractMeta()

names(meta)

# Lots of good stuff there including longitude and latitude

# Lets use our getVariable() Function to pull out a variable

epaRegion <- getVariable(
  meta$longitude, 
  meta$latitude, 
  dataset = "EPARegions_01", 
  variable = "epaRegion"
)

# Let's create a simplified combined dataframe for comparison

meta$epaRegion <- epaRegion

meta %>%
  select(siteName, stateCode, countyName, epaRegion) %>%
  View()

# Looks good.

