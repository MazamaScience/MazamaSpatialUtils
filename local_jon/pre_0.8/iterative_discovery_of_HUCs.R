# Can we identify large scale HUCs first to reduce the number of small scale
# HUCs we need to seach through?

library(MazamaSpatialUtils)

setSpatialDataDir('~/Data/Spatial')

# Load several HUC datasets
loadSpatialData('WBDHU4')
loadSpatialData('WBDHU8')
loadSpatialData('WBDHU10')

# Create a vector 601 locations
lon <- seq(-124, -118, 0.01)
lat <- seq(44, 50, 0.01)

WA4 <- subsetHUC(WBDHU4, allStateCode='WA')
WA8 <- subsetHUC(WBDHU8, allStateCode='WA')
WA10 <- subsetHUC(WBDHU10, allStateCode = 'WA')

# Iterative discovery of HUC10s
iterative_HU10_codes <- function() {
  HU4Codes <- getHUC(lon, lat, "WA4")
  HUC8Subset <- subsetHUC(WA8, parentHUCs = sort(unique(HU4Codes)))
  HU8Codes <- getHUC(lon, lat, "HUC8Subset")
  HUC10Subset <- subsetHUC(WA10, parentHUCs = sort(unique(HU8Codes)))
  HU10Codes <- getHUC(lon, lat, "HUC10Subset")
  return(HU10Codes)
}

# Look through all HUC10s
brute_force_HU10_codes <- function() {
  HU10Codes <- getHUC(lon, lat, "WA10")
  return(HU10Codes)
}

# TESTING
if (FALSE) {
  
  # How fast is this?
  HU10Codes_B <- brute_force_HU10_codes()
  SFDF_B <- subsetHUC(WA10, parentHUCs = HU10Codes_B)
  plot(SFDF_B)
  plot(SFDF_B, border='cyan', add=T)
  
  # Is this faster? (NO)
  HU10Codes_A <- iterative_HU10_codes()
  SFDF_A <- subsetHUC(WA10, parentHUCs = HU10Codes_A)
  plot(SFDF_A)
  points(lon, lat, col='red', pch=16, cex=0.5, xpd=NA)
  
}
