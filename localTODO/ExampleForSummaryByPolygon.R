library(MazamaSpatialUtils)
library(sp)

###download.file('https://www.fhwa.dot.gov/bridge/nbi/2016hwybronlyonefile.zip','nbi_2016.zip')
nbi <- readr::read_csv('nbi_2016.zip')

setSpatialDataDir("~/Data/Spatial")
###installSpatialData()
loadSpatialData("NaturalEarthAdm1")
loadSpatialData("USCensusCounties")
loadSpatialData("WBDHU6")
loadSpatialData("WBDHU8")

# Store the columns we need in a dataframe
nbiDF <- data.frame(longitude=-as.numeric(nbi$LONG_017)/1000000,
                    latitude=as.numeric(nbi$LAT_016)/1000000,
                    value=2017-as.numeric(nbi$YEAR_BUILT_027)+1)

# Get only the CONUS
longIndex <- intersect(which(nbiDF$longitude > -130), which(nbiDF$longitude < -30))
latIndex <- intersect(which(nbiDF$latitude > 20), which(nbiDF$latitude < 50))
indexes <- intersect(longIndex, latIndex)
nbiDF <- nbiDF[indexes,]

# Sample the indexes to speed up summaryByPolygon
sampleIndex <- sample(nrow(nbiDF), 10000)

usPolygon <- NaturalEarthAdm1[NaturalEarthAdm1$countryCode == 'US',]

# Get summaried values by state
source('localTODO/summaryByPolygon.R')
df <- summaryByPolygon(nbiDF$longitude[sampleIndex], nbiDF$latitude[sampleIndex],
                       nbiDF$value[sampleIndex], usPolygon, 'code_hasc', mean)
df <- na.omit(df)
states <- sapply(df[,1],function(x){stringr::str_split_fixed(x, 'US.', 2)[2]})

statePolygon <- usPolygon[usPolygon$stateCode %in% states,]

# Get the correct plot order
plotDF <- data.frame(state=states, value=df[,2])
plotOrder <- usPolygon$stateCode[usPolygon$stateCode %in% states]
plotOrder <- as.data.frame(plotOrder)
names(plotOrder) <- "state"
plotOrder$state <- as.character(plotOrder$state)
plotDF <- dplyr::left_join(plotOrder, plotDF, by='state')

# Plot colors by quantiles
breaks <- quantile(df$summaryValue)
colIndexes <- .bincode(plotDF$value, breaks)
colors <- RColorBrewer::brewer.pal(4, 'Blues')
cols <- colors[colIndexes]

plot(statePolygon, col=cols)

#####  Now only look at WA, and we take summarized values by county, HUC6 and HUC8
# Note: Washington State Latitude 45° 33′ N to 49° N and Longitude 116° 55′ W to 124° 46′ W
# Subset nbi to get only WA
longIndex <- intersect(which(nbiDF$longitude > -125), which(nbiDF$longitude < -117))
latIndex <- intersect(which(nbiDF$latitude > 46), which(nbiDF$latitude < 49))
indexes <- intersect(longIndex, latIndex)
nbiWA <- nbiDF[indexes,]

# Subset the polygon that will be used
usPolygon <- USCensusCounties[USCensusCounties$countryCode == 'US',]
waPolygon <- usPolygon[usPolygon$stateCode == 'WA',]

# Get summaried values by WA county
df <- summaryByPolygon(nbiWA$longitude, nbiWA$latitude, nbiWA$value, waPolygon, 'countyName', mean)
df <- na.omit(df)

# Get the correct plot order
plotOrder <- waPolygon$countyName[waPolygon$countyName %in% df$polyID]
plotOrder <- as.data.frame(plotOrder)
names(plotOrder) <- "polyID"
plotDF <- dplyr::left_join(plotOrder, df, by='polyID')

# Plot colors by quantiles
breaks <- quantile(df$summaryValue)
colIndexes <- .bincode(plotDF$summaryValue, breaks)
colors <- RColorBrewer::brewer.pal(4, 'Blues')
cols <- colors[colIndexes]

plot(waPolygon, col=cols)

# Now we can take a look at summarized value by watershed
usHUC6 <- WBDHU6[WBDHU6$countryCode == 'US',]
waHUC6 <- usHUC6[usHUC6$stateCode == 'WA',]

# Get summaried values by HUC6
df <- summaryByPolygon(nbiWA$longitude, nbiWA$latitude, nbiWA$value, waHUC6, 'HUC', mean)
df <- na.omit(df)

# Get the correct plot order
plotOrder <- waHUC6$HUC[waHUC6$HUC %in% df$polyID]
plotOrder <- as.data.frame(plotOrder)
names(plotOrder) <- "polyID"
plotDF <- dplyr::left_join(plotOrder, df, by='polyID')

# Plot colors by quantiles
breaks <- quantile(df$summaryValue)
colIndexes <- .bincode(plotDF$summaryValue, breaks)
colors <- RColorBrewer::brewer.pal(4, 'Blues')
cols <- colors[colIndexes]

plot(waHUC6, col=cols)

# Lastly, we can examine how HUC6 differ from HUC8
usHUC8 <- WBDHU8[WBDHU8$countryCode == 'US',]
waHUC8 <- usHUC8[usHUC8$stateCode == 'WA',]

# Get summaried values by HUC6
df <- summaryByPolygon(nbiWA$longitude, nbiWA$latitude, nbiWA$value, waHUC8, 'HUC', mean)
df <- na.omit(df)

# Get the correct plot order
plotOrder <- waHUC8$HUC[waHUC8$HUC %in% df$polyID]
plotOrder <- as.data.frame(plotOrder)
names(plotOrder) <- "polyID"
plotDF <- dplyr::left_join(plotOrder, df, by='polyID')

# Plot colors by quantiles
breaks <- quantile(df$summaryValue)
colIndexes <- .bincode(plotDF$summaryValue, breaks)
colors <- RColorBrewer::brewer.pal(4, 'Blues')
cols <- colors[colIndexes]

plot(waHUC8, col=cols)

# Read in external shapefile and apply summaryByPolygon

download.file("http://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_cd115_500k.zip",
              destfile = "cb_2016_us_cd115_500k.zip")
unzip("cb_2016_us_cd115_500k.zip")
us_map <- rgdal::readOGR("cb_2016_us_cd115_500k.shp", layer = "cb_2016_us_cd115_500k")

WA_leg <- subset(us_map, STATEFP == "53")
WA_leg$CD115FP <- as.character(WA_leg$CD115FP)
WA_df <- summaryByPolygon(nbiWA$longitude, nbiWA$latitude, nbiWA$value,
                          WA_leg, 'CD115FP', median)
WA_df <- na.omit(WA_df)

# Get the correct plot order
plotOrder <- WA_leg$CD115FP[WA_leg$CD115FP %in% WA_df$polyID]
plotOrder <- as.data.frame(plotOrder)
names(plotOrder) <- "polyID"
plotDF <- dplyr::left_join(plotOrder, WA_df, by='polyID')

# Plot colors by quantiles
breaks <- quantile(WA_df$summaryValue)
colIndexes <- .bincode(plotDF$summaryValue, breaks)
colors <- RColorBrewer::brewer.pal(4, 'Blues')
cols <- colors[colIndexes]

plot(WA_leg, col=cols)
