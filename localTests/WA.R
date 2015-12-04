

library(sp) 
library(MazamaSpatialUtils)

setSpatialDataDir('~/Data/Spatial')

loadSpatialData('USGSHUC4')
loadSpatialData('USGSHUC8')
loadSpatialData('USGSHUC10')

lon <- seq(-124, -118, 0.01)
lat <- seq(44, 50, 0.01)

WA4 <- subsetHUC(USGSHUC4, allStateCode='WA')
WA8 <- subsetHUC(USGSHUC8, allStateCode='WA')
WA10 <- subsetHUC(USGSHUC10, allStateCode = 'WA')

# Iterative discovery of HUCs
bop <- function() {
  HUC4Codes <- getHUC(lon, lat, WA4)
  bop8 <- subsetHUC(WA8, parentHUCs = sort(unique(HUC4Codes)))
  HUC8Codes <- getHUC(lon, lat, bop8)
  bop10 <- subsetHUC(WA10, parentHUCs = sort(unique(HUC8Codes)))
  HUC10Codes <- getHUC(lon, lat, bop10)
  return(HUC10Codes)
}

bip <-function() {
  HUC10Codes <- getHUC(lon, lat, WA10)
  return(HUC10Codes)
}

# TESTING
if (FALSE) {
  
  HUCbop <- bop()
  SPDFbop <- subsetHUC(WA10, parentHUCs = HUCbop)
  plot(SPDFbop)
  points(lon, lat, col='red', pch=16, xpd=NA)
  HUCbip <- bip()
  SPDFbip <- subsetHUC(WA10, parentHUCs = HUCbip)
  plot(SPDFbip, border='cyan', add=T)
  
}
