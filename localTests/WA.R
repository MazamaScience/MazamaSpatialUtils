

library(sp) 
library(MazamaSpatialUtils)

setSpatialDataDir('~/Data/Spatial')

loadSpatialData('WBDHU4')
loadSpatialData('WBDHU8')
loadSpatialData('WBDHU10')

lon <- seq(-124, -118, 0.01)
lat <- seq(44, 50, 0.01)

WA4 <- subsetHUC(WBDHU4, allStateCode='WA')
WA8 <- subsetHUC(WBDHU8, allStateCode='WA')
WA10 <- subsetHUC(WBDHU10, allStateCode = 'WA')

# Iterative discovery of HUs
bop <- function() {
  HU4Codes <- getHUC(lon, lat, WA4)
  bop8 <- subsetHUC(WA8, parentHUCs = sort(unique(HU4Codes)))
  HU8Codes <- getHUC(lon, lat, bop8)
  bop10 <- subsetHUC(WA10, parentHUCs = sort(unique(HU8Codes)))
  HU10Codes <- getHUC(lon, lat, bop10)
  return(HU10Codes)
}

bip <-function() {
  HU10Codes <- getHUC(lon, lat, WA10)
  return(HU10Codes)
}

# TESTING
if (FALSE) {
  
  HUbop <- bop()
  SPDFbop <- subsetHUC(WA10, parentHUCs = HUbop)
  plot(SPDFbop)
  points(lon, lat, col='red', pch=16, xpd=NA)
  HUbip <- bip()
  SPDFbip <- subsetHUC(WA10, parentHUCs = HUbip)
  plot(SPDFbip, border='cyan', add=T)
  
}
