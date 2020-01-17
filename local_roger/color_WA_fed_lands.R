library(MazamaSpatialUtils)
library(dplyr)

setSpatialDataDir("~/Data/Spatial")
loadSpatialData("USCensusStates")

# Color the Federal lands in WA state by agency that manages them

# Get some state outlines
wa <- subset(USCensusStates, stateCode == "WA")

# Load the HIFLDFederalLands data set
loadSpatialData("HIFLDFederalLands")

# What fields do we have?
names(HIFLDFederalLands@data)
#  [1] "areaID"             "primaryLandType"    "primaryLandOwner"   "secondaryLandType"
#  [5] "secondaryLandOwner" "tertiaryLandType"   "tertiaryLandOwner"  "agencyCode"
#  [9] "areaURL"            "primaryName"        "secondaryName"      "tertiaryName"
# [13] "allStateCodes"      "longitude"          "latitude"           "stateCode"
# [17] "countryCode"

# Get a subset of just the fed. lands in the state of WA
wa_fed <- subset(HIFLDFederalLands, stateCode == "WA")

# Throw a quick plot on the screen
plot(wa)
plot(wa_fed, add = TRUE, col = "green")

# What are the unique types of federal lands in WA?  Look in "primaryLandType"
print(select(wa_fed@data, primaryLandType) %>% group_by(primaryLandType) %>% summarise(n()))

# 1 Air Force                   10
# 2 Army                        10
# 3 Army Corps of Engineers      3
# 4 Bureau of Reclamation       11
# 5 Department of Energy         1
# 6 National Fish Hatchery       2
# 7 National Forest             69
# 8 National Historic Park       2
# 9 National Monument            1
#10 National Park                5
#11 National Recreation Area     2
#12 National Reserve             1
#13 National Scenic Area         2
#14 National Wildlife Refuge    29
#15 Navy                        11
#16 Public Domain Land         179
#17 U.S. Coast Guard             1
#18 Wilderness                  46

# What are the agencies that manage land in WA?
print(select(wa_fed@data, primaryLandOwner) %>% group_by(primaryLandOwner) %>% summarise(n()))

# 1 BLM                180
# 2 BOR                 11
# 3 DOD                 34
# 4 DOE                  1
# 5 DOT                  1
# 6 FS                 108
# 7 FWS                 31
# 8 NPS                 19

# Color the WA Fed Land map by agency
agency_idx <- as.factor(wa_fed@data$primaryLandOwner)

# Have to order the levels in the index if we want the areas colored in the same order
agency_idx <- factor(agency_idx, levels = c("FS", "BLM", "DOD", "FWS", "NPS", "BOR", "DOE", "DOT"))
color_list <- c("forestgreen", "purple", "yellow", "blue", "brown", "gray", "red", "orange")

plot(wa_fed, add = TRUE, col = color_list[agency_idx])

# Add a legend that matched color to agency
legend("bottomleft", fill = color_list, legend = as.character(unique(agency_idx)))

