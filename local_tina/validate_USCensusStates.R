# Validating USCensusStates 

#load in packages
library(MazamaSpatialUtils)
library(sp)

#setSpatialDataDir("~/Data/Spatial")
setSpatialDataDir('~/SpatialData')

convertUSCensusStates()

loadSpatialData("USCensusStates")
