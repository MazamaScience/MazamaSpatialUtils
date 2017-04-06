library(MazamaSpatialUtils)
library(PWFSLSmoke)
library(sp)

# Load data
setSpatialDataDir("~/Data/Spatial")
loadSpatialData("NaturalEarthAdm1")

# Choose a set of states in the US to examine
countryCodes <- "US"
# stateCodes <- c("WA", "ID", "OR", "NV", "CA")
stateCodes <- c("CT", "NY", "MA", "RI", "PA", "NJ")

# Get the most recent one day's data in UTC tz from airnow
airnow <- airnow_loadLatest()
today <- lubridate::today()
today <- lubridate::with_tz(today,"UTC")
tmr <- today+1
today <- format(today, "%Y%m%d")
tmr <- format(tmr, "%Y%m%d")

airnowOneday <- monitor_subset(airnow, tlim=c(today,tmr), stateCodes=stateCodes)

countryPolygon <- NaturalEarthAdm1[NaturalEarthAdm1$countryCode %in% countryCodes,]
statePolygon <- countryPolygon[countryPolygon$stateCode %in% stateCodes,]

# Create a dataframe that has stateCodes and values
meanValues <- apply(airnowOneday$data[,-1], 2, function(x){mean(x, na.rm=TRUE)})
DF <- data.frame(stateCode=airnowOneday$meta$stateCode, value=meanValues)

# Group values by state and calculate the means
byState <- dplyr::group_by(DF, stateCode)
meanDF <- as.data.frame(summarise(byState, mean(value)))
names(meanDF)[2] <- "meanValue"
meanDF[,1] <- as.character(meanDF[,1])

# Sort the dataframe according to the plotOrder in NaturalEarthAdm1
plotOrder <- countryPolygon$stateCode[countryPolygon$stateCode %in% stateCodes]
plotOrder <- as.data.frame(plotOrder)
names(plotOrder) <- "stateCode"
plotOrder[,1] <- as.character(plotOrder[,1])
meanDF <- dplyr::left_join(plotOrder, meanDF, by="stateCode")

# Create colors and bin the mean values by the break points
breaks <- c(1,5,10,20,40,90,140,350,525)
breaks <- c(min(c(0,min(meanDF$meanValue)),na.rm=TRUE),breaks)
breaks <- c(breaks,max(c(600,max(meanDF$meanValue)),na.rm=TRUE))
red <- c(255,255,255,255,255,255,255,200,150)/255
green <- c(225,195,165,135,105,75,46,2,3)/255
blue <- c(225,195,165,135,105,75,45,3,3)/255
colors <- c('transparent',grDevices::rgb(red=red, green=green, blue=blue))
colIndexes <- .bincode(meanDF$meanValue, breaks=breaks)

# Draw the spatialPolygon and color them by different values
plot(statePolygon, col=colors[colIndexes])

# Get a legend for the plot
legendText <- character(length=length(breaks)-1)
for( i in 1:length(legendText) ) {
  legendText[i] <- paste0(breaks[i], ' - ', breaks[i+1])
}
uniqueIndexes <- unique(colIndexes)
legendCols <- colors[uniqueIndexes]
legendText <- legendText[uniqueIndexes]

legend('topleft', legend=legendText, col=legendCols, pch=16, pt.cex=1.5, bty='n', title='Mean PM2.5 Value')
