# findCountries demo

library(MazamaSpatialUtils)
library(sf)

# Vector of lons and lats
lons <- seq(5,25,2)
lats <- seq(30,50,2)

# Get country names
countryNames <- getCountryName(lons, lats, dataset = 'SimpleCountries')
print(countryNames)

# Get all information in the dataset
countryDF <- getCountryName(lons, lats, dataset = 'SimpleCountries', allData = TRUE)
print(countryDF)

# Subset the simple features data frame to only include our countries
countryMask <- SimpleCountries$countryName %in% countryNames

# Plot the country polygons
plot(SimpleCountries$geometry[countryMask], col = 'gray90', border = 'gray70')
# Add all country boundaries
plot(SimpleCountries$geometry, border = 'gray80', add = TRUE)
# Add our points in red
points(lons,lats, pch = 16, col = 'red')
# Add text to the right
countryText <- ifelse(is.na(countryNames), 'water', paste0(countryDF$countryCode, ' = ', countryDF$countryName))
text(lons, lats, countryText, pos = 4)
# Add a title
title('Country Codes and Names')

# Now use SimpleCountriesEEZ
countryNames <- getCountryName(lons, lats, dataset = 'SimpleCountriesEEZ')
countryDF <- getCountryName(lons, lats, dataset = 'SimpleCountriesEEZ', allData = TRUE)
countryMask <- SimpleCountriesEEZ$countryName %in% countryNames

# Plot the country polygons
plot(SimpleCountriesEEZ$geometry[countryMask],col = 'gray90', border = 'gray70')
# Add all country boundaries
plot(SimpleCountries$geometry, border = 'gray80', add = TRUE)
# Add our points in red
points(lons, lats, pch = 16, col = 'red')
# Add text to the right
countryText <- ifelse(is.na(countryNames), 'water', paste0(countryDF$countryCode, ' = ', countryDF$countryName))
text(lons, lats, countryText, pos = 4)
# Add a title
title('Country Codes and Names (using EEZ boundaries)')

