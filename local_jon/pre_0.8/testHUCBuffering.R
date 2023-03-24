# ----- Get HUC data -------------------------------------------------- 
# https://www.usgs.gov/core-science-systems/ngp/national-hydrography/access-national-hydrography-products
# data http://prd-tnm.s3-website-us-west-2.amazonaws.com/?prefix=StagedProducts/Hydrography/WBD/HU2/Shape/WBD_01_HU2_Shape.zip

# load HUC dataset
loadSpatialData('WBDHU8.RData')

#create latitude and longitude vectors with one point in and one point out
hucLongitude <- c(-68.05, -70.0)
hucLatitude <- c(43.92, 44)

# plot, showing one data point in the domain and one outside of the domain of HUC data
plot(WBDHU8)
points(longitude[2], latitude[2], pch = 13, col = 'green')
points(longitude[1], latitude[1], pch = 13, col = 'red')

# ----- Test HUC functions -------------------------------------------------- 

#test getHUC with and without buffering
getHUC_nobuff <- getHUC(
  lon = hucLongitude, 
  lat = hucLatitude, 
  dataset = "WBDHU8", 
  HUCs = NULL, 
  allData = FALSE,
  useBuffering = FALSE
)

getHUC_withbuff <- getHUC(
  lon = longitude, 
  lat = latitude, 
  dataset = "WBDHU8", 
  HUCs = NULL, 
  allData = FALSE,
  useBuffering = TRUE
)

# View results
View(getHUC_nobuff)
View(getHUC_withbuff)

# Test the getHUCName function with and without buffering
getHUCName_nobuff <- getHUCName(
  lon = longitude, 
  lat = latitude, 
  dataset = "WBDHU8", 
  HUCs = NULL, 
  allData = FALSE,
  useBuffering = FALSE
)

getHUCName_withbuff <- getHUCName(
  lon = longitude, 
  lat = latitude, 
  dataset = "WBDHU8", 
  HUCs = NULL, 
  allData = FALSE,
  useBuffering = TRUE
)

# View results
View(getHUCName_nobuff)
View(getHUCName_withbuff)

# ----- Test getVariable -------------------------------------------------- 
#load state data
loadSpatialData('USCensusStates')

# get Maine and Mass data
MEMA <- subset(USCensusStates, stringr::str_detect(USCensusStates@data$stateCode, "ME") |
                 stringr::str_detect(USCensusStates@data$stateCode, "MA")
)

MEMA_01 <- subset(USCensusStates_01, stringr::str_detect(USCensusStates_01@data$stateCode, "ME") |
                    stringr::str_detect(USCensusStates_01@data$stateCode, "MA")
)

# Define longitude and latitude points both in and out of the MEMA_01 domain 
stateLongitude <- c(-68.8405, -70.0, -70.1, -72)
stateLatitude <- c(43.9237, 44, 42.01, 42.5)

# Plot to show where the points are 
# clearly all points are part of the State territories as seen in the high 
# resolution dataset (red border). But two are outside the bounds in the low 
# resolution dataset.
plot(MEMA, border = "red")
plot(MEMA_01, border = "black", add = TRUE)
points(longitude, latitude, pch = 13, col = 'blue')

# Test getVariable using with and without buffering
# using getVariable to get StateCode from USCensusStates_01
getVariable_nobuff <- getVariable(
  lon = stateLongitude, 
  lat = stateLatitude, 
  dataset = 'USCensusStates_01', 
  variable = 'stateCode', 
  countryCodes = NULL, 
  allData = FALSE,
  useBuffering = FALSE
)

getVariable_withbuff <- getVariable(
  lon = longitude, 
  lat = latitude, 
  dataset = 'USCensusStates_01', 
  variable = 'stateCode', 
  countryCodes = NULL, 
  allData = FALSE,
  useBuffering = TRUE
)

#view results
View(getVariable_nobuff)
View(getVariable_withbuff)


