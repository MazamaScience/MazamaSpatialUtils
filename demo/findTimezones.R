# findTimezones demo

library(MazamaSpatialUtils)
library(sf)

# Vector of lons and lats
lons <- seq(-120,-60,5)
lats <- seq(20,80,5)

# Get Olson timezone names
timezones <- getTimezone(lons, lats)
print(timezones)

# Get all information in the dataset
timezoneDF <- getTimezone(lons, lats, allData=TRUE)
print(timezoneDF)

# Subset the simple features data frame to only include our timezones
timezoneMask <- SimpleTimezones$timezone %in% timezones

# Plot the timezone polygons
plot(SimpleTimezones$geometry[timezoneMask],col='gray90',border='gray70')
# Add all country boundaries
plot(SimpleCountries$geometry, border = 'gray80', add = TRUE)
# Add our points in red
points(lons,lats,pch=16,col='red')
# Add text to the right
timezoneText <- ifelse(is.na(timezones),'water',timezones)
text(lons,lats,timezoneText,pos=4)
# Add a title
title('Timezones in North America')

