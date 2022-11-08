# findStates demo

library(MazamaSpatialUtils)
library(sf)

# This demo only works if spatial has been installed in '~/Data/Spatial'
setSpatialDataDir('~/Data/Spatial_0.8')

# Load the data
result <- try( loadSpatialData('NaturalEarthAdm1.rda') )

if ( "try-error" %in% class(result) ) {
  stop('This demo requires default installation directories for spatial data. See installSpatialData().')
}

# Vector of lons and lats
lons <- seq(-120,-60,5)
lats <- seq(20,80,5)

# Get state names
stateNames <- getStateName(lons, lats)
print(stateNames)

# Get all information in the dataset
stateDF <- getStateName(lons, lats, allData=TRUE)
print(stateDF)

# Subset the simple features data frame to only include our states
# NOTE:  Need to be careful to remove polygons with stateName == NA
stateMask <- NaturalEarthAdm1$stateName %in% stateNames & !is.na(NaturalEarthAdm1$stateName)

# Plot the timezone polygons
plot(NaturalEarthAdm1$geometry[stateMask],col='gray90',border='gray70')
# Add countries from the SimpleCountries dataset
plot(SimpleCountries$geometry,add=TRUE,border='gray80')
# Add our points in red
points(lons,lats,pch=16,col='red')
# Add text to the right
stateText <- ifelse(is.na(stateNames),'water',paste0(stateDF$stateCode,' = ',stateDF$stateName))
text(lons,lats,stateText,pos=4)
# Add a title
title('States & Provinces in North America')

# ----- Show the dangers of working with state codes ---------------------------

# Vector of lons and lats
lons <- seq(-120,-60,5)
lats <- seq(20,80,5)

# Get state names
stateCodes <- getStateCode(lons, lats)

# Subset the simple features data frame to only include our states
# NOTE:  Need to be careful to remove polygons with stateCode == NA
stateMask <- NaturalEarthAdm1$stateCode %in% stateCodes & !is.na(NaturalEarthAdm1$stateCode)

# Plot the timezone polygons
plot(NaturalEarthAdm1$geometry[stateMask],col='gray90',border='gray70')
# Add countries from the SimpleCountries dataset
plot(SimpleCountries$geometry,add=TRUE,border='gray80')
# Add our points in red
points(lons,lats,pch=16,col='red')
# Add text to the right
stateText <- ifelse(is.na(stateNames),'water',paste0(stateDF$stateCode,' = ',stateDF$stateName))
text(lons,lats,stateText,pos=4)
# Add a title
title('**WARNING** 2-Character State Codes are not Unique!')
uniqueCodesText <- paste(sort(unique(stateCodes[!is.na(stateCodes)])),collapse=', ')
title(line=0,paste0('Showing all polygons with "stateCode" matching ',uniqueCodesText))
