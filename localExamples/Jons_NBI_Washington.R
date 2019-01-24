# Testing summarizeByPolygon

# Using NBI code and data from Jon's desktop machine
source('~/Projects/MazamaScience/NBI/R/convert2016.R')
nbi <- convert2016('~/Downloads/2016hwybronlyonefile.zip')

# Subset NBI and spatial data to focus on Washington
nbi_wa <- nbi[nbi$stateCode == 'WA',]
nbi_wa <- nbi_wa[nbi_wa$longitude < -100,] # bad value needs to be cleaned up in NBI::convert2016()

plot(nbi_wa$longitude, nbi_wa$latitude)

loadSpatialData('USCensusCounties')
wa <- subset(USCensusCounties, stateCode == 'WA')

plot(wa)
points(nbi_wa$longitude, nbi_wa$latitude, pch=0)

# Summarize and add this data column to the SPDF dataframe
bop <- summarizeByPolygon(nbi_wa$longitude, nbi_wa$latitude, nbi_wa$yearBuilt, wa, FUN=mean)
wa@data$yearBuilt <- dplyr::left_join(wa@data, bop, by="polygonID")
wa@data$yearBuilt

# TODO:  Should summarizeByPolygon return an SPDF with a new column of data?

# Example for a talk
nbi <- nbi_wa
library(dplyr)
loadSpatialData('USCensusCounties')
wa <- subset(USCensusCounties, stateCode == 'WA')

ageByPolygon <- summarizeByPolygon(nbi$longitude,
                                   nbi$latitude,
                                   nbi$age,
                                   wa,
                                   FUN=mean)

left_join(wa@data, ageByPolygon, by="polygonID") %>%
  select(countyName, summaryValue) %>% 
  arrange(summaryValue) %>%
  rename(county = countyName,
         mean_age = summaryValue)
  

