# Testing the USCensusCBSA dataset

library(MazamaSpatialUtils)
setSpatialDataDir("~/Data/Spatial")

loadSpatialData("USCensusCBSA")

# Looking in the Environment tab shows USCensusCBSA at 52.9 MB -- Too big to plot!

plot(USCensusCBSA_01)

# Nice!

# Let's create a subset to work with

wa <- subset(USCensusCBSA, stringr::str_detect(USCensusCBSA@data$allStateCodes, "WA"))
wa_05 <- subset(USCensusCBSA_05, stringr::str_detect(USCensusCBSA_05@data$allStateCodes, "WA"))
wa_02 <- subset(USCensusCBSA_02, stringr::str_detect(USCensusCBSA_02@data$allStateCodes, "WA"))
wa_01 <- subset(USCensusCBSA_01, stringr::str_detect(USCensusCBSA_01@data$allStateCodes, "WA"))

# How much simplification is "too much"

plot(wa, border = "red")
plot(wa_05, border = "black", add = TRUE)

# No difference at all.

plot(wa, border = "red")
plot(wa_01, border = "black", add = TRUE)

# Only super tiny differences along mountain ridges or rivers.

# This is a dataset where the 1% version is good enough most of the time, 
# especially for plotting.

# Let's grab some monitoring data and extract the associated CBSAName

library(PWFSLSmoke)

wa_meta <- 
  monitor_loadLatest() %>%
  monitor_subset(stateCodes = "WA") %>%
  monitor_extractMeta()

names(wa_meta)

# Lots of good stuff there including longitude and latitude

# Lets use our getVariable() Function to pull out a variable

names()

CBSAName <- getVariable(
  wa_meta$longitude, 
  wa_meta$latitude, 
  dataset = "wa_01", 
  variable = "CBSAName"
)

# Let's create a simplified combined dataframe for comparison

wa_meta$cbsaName <- CBSAName

wa_meta %>%
  select(siteName, stateCode, countyName, msaName, cbsaName) %>%
  View()

# Nice! cbsaName is better than msaName

# But some locations are still missing:
#  Omak, Twisp, Port Townsend, Raymond, Winthrop, Dayton

# Those are all tiny towns so it sounds like things are working appropriately

# Lets add a map to visualize

library(dplyr)

plot(wa_01)
points(wa_meta$longitude, wa_meta$latitude)
wa_meta %>% 
  filter(is.na(cbsaName)) %>% 
  select(longitude, latitude) %>% 
  points(pch = 16, col = 'red')

# Nice!

