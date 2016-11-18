---
title: "Introduction to MazamaSpatialUtils"
author: "Jonathan Callahan"
date: "2016-11-17"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Introduction to MazamaSpatialUtils}
  %\VignetteEngine{knitr::rmarkdown}
  \usepackage[utf8]{inputenc}
---

## Background

The MazamaSpatialUtils package was created by [MazamaScience](http://mazamascience.com) to 
regularize our work with spatial data. The **sp**, **rgdal** and **maptools** packages have 
made it much easier to work with spatial data found in shapefiles. Many sources of shapefile 
data are available and can be used to make beautfiul maps in R. Unfortunately, the data attached 
to these datasets, even when fairly complete, often lacks standardized identifiers such as the 
ISO 3166-1 alpha-2 encodings for countries. Maddeningly, even when these ISO codes are used, 
the dataframe column in which they are stored does not have a standardized name. It may be
called `ISO` or `ISO2` or `alpha` or `COUNTRY` or any of a dozen other names we have seen.

While many mapping packages provide 'natural' naming of countries, those who wish to develop 
operational, GIS-like systems need something that is both standardized and language-independent. 
The ISO 3166-1 alpha-2 encodings have emerged as the *defacto* standard for this sort of work. 
In similar fashion, ISO 3166-2 alpha-2 encodings are aviailable for the next administrative 
level down -- state/province/oblast, *etc.*. For timezones, the *defacto* standard is the set 
of Olson timezones used in all UNIX systems.

The main goal of this package is to create an internally standardized set of spatial data 
that we can use in various projects. Along with two built-in datasets, this package provides 
'convert~' functions for other spatial datasets that we currently use. These convert functions 
all follow the same recipe:

* download spatial data in shapefile format into a standard directory
* convert shapefile data into a *sp* SpatialPolygonsDataFrame
* modify the dataframe in the @data slot so that it adheres to package internal standards

Other datasets can be added following the same procedure.

The 'package internal standards' are very simple. Every spatial dataset will have at least one 
of the following, consistently named colums of data:

* countryCode -- ISO 3166-1 alpha-2
* stateCode -- ISO 3166-2 alpha-2
* timezone -- Olson timezone

If another column contains this data, that column must be renamed or duplicated with the 
internally standardized name. This simple level of consistency makes it posisble to generate 
maps for any data that is ISO encoded. It also makes it possible to create functions that 
return the country, state or timezone associated with a set of locations.

## Functionality

The core functionality for which this package was developed is determining spatial information 
associated with a set of locations.

Current functionality includes the following:

* `getCountry~(lon,lat)` -- returns names, ISO codes and other country-level data specified locations
* `getState~(lon,lat)` -- returns names, ISO codes and other state-level at specified locations
* `getUSCounty~(lon,lat)` -- returns names and other county-level data at specified locations
* `getTimezones(lon,lat)` -- returns Olson timezones and other data at specified locations

For those working with geo-located data, this information is key.


## Standard Datasets and Setup

When using MazamaSpatialUtils, always run `setSpatialDataDir('SOME_DIRECTORY')` first. This 
sets the directory where spatial data will be installed and loaded from.

MazamaSpatialUtils has 2 built-in datasets:

* `SimpleCountries`
* `SimpleTimezones`

Version 0.1 of the package is built around the two internal datasets and five other core 
datasets that may be installed: 

* `NaturalEarthAdm1`
* `TMWorldBorders`
* `TMWorldBordersSimple`
* `USCensusCounties`
* `WorldTimezones`

These datasets are not included in the package. You must instead run `initializeSpatialData()` 
which will download each dataset into your data directory as `.RData` files.


```r
library(MazamaSpatialUtils)

# Set data directory to working directory
setSpatialDataDir('./SpatialData')
initializeSpatialData()
```

You may also install datasets individually.


```r
convertNaturalEarthAdm1()
convertTMWorldBorders()
convertTMWorldBordersSimple()
convertUSCensusCounties()
convertWorldTimezones()
```

## Loading Datasets

Running `initializeSpatialData()` will also load each dataset into the environment. 
If you installed a dataset individually or are coming back to a project, load a dataset 
with the `loadSpatialData(DATASET)` function.


```r
loadSpatialData('NaturalEarthAdm1')
loadSpatialData('TMWorldBorders')
loadSpatialData('TMWorldBordersSimple')
loadSpatialData('USCensusCounties')
loadSpatialData('WorldTimezones')
```

## `getCountry()` and `getCountryCode()`

These two functions are used for finding which country one or many spatial points are in. 
`getCountry()` returns English country names and `getCountryCode()` returns the ISO-3166 two 
character country code. Both functions can be passed `allData = TRUE` which returns a dataframe 
with more information on the countries. You can also specify `countryCodes = c(CODES)` to speed
up searching.

These functions use the package-internal `SimpleCountries` dataset which can be used without 
loading any additional datasets.

In this example we'll find which countries a vector of points fall in. 


```r
library(MazamaSpatialUtils)

lon <- c(-122.3, -73.5, 21.1, 2.5)
lat <- c(47.5, 40.75, 52.1, 48.5)

getCountry(lon, lat)
```

```
## [1] "United States" "United States" "Poland"        "France"
```

```r
getCountryCode(lon, lat)
```

```
## [1] "US" "US" "PL" "FR"
```

```r
getCountry(lon, lat, allData=TRUE)
```

```
##   FIPS countryCode ISO3 UN_country   countryName        area
## 1   US          US  USA        840 United States 9.15896e+12
## 2   US          US  USA        840 United States 9.15896e+12
## 3   PL          PL  POL        616        Poland 3.06290e+11
## 4   FR          FR  FRA        250        France 5.50100e+11
##   population2005 UN_region UN_subregion longitude latitude
## 1      299846449        19           21   -98.606   39.622
## 2      299846449        19           21   -98.606   39.622
## 3       38195558       150          151    19.401   52.125
## 4       60990544       150          155     2.550   46.565
```

## `getState()` and `getStateCode()`

Similar to above, these functions return state names and ISO 3166 code. They also take 
the same arguments. Adding the `countryCodes` argument is more important for `getState()` 
and `getStateCode()` because the `NaturalEarthAdm1` dataset is fairly large. Lets use the 
same `lat` and `lon` variables as above and find out which states those points are in.

These functions require installation of the large `NaturalEarthAdm1` dataset which is not 
distributed with the package.


```r
# Load states dataset if you haven't already
loadSpatialData("NaturalEarthAdm1")

# Get which countries the points are in
countryCodes <- getCountryCode(lon, lat)

# Pass the country codes as an argument to speed everything up
getState(lon, lat, countryCodes = countryCodes)

getStateCode(lon, lat, countryCodes = countryCodes)

# This is a very detailed dataset so we'll grab a few important columns
states <- getState(lon, lat, allData=TRUE, countryCodes = countryCodes)
states[c('countryCode', 'stateCode', 'stateName')]
```

## `getTimezone()`

Returns the Olsen Timezone where the given points are located. Arguments are the same as 
the previous functions. `allData=TRUE` will return other useful information such as the UTC Offset.

These functions use the package-internal `SimpleTimezones` dataset which can be used 
without loading any additional datasets.


```r
# Find the timezones the points are in
getTimezone(lon, lat)
```

```
## [1] "America/Los_Angeles" "America/New_York"    "Europe/Warsaw"      
## [4] "Europe/Paris"
```

```r
# Find which countries the points are in
countryCodes <- getCountryCode(lon, lat)

# Pass the country codes as an argument to potentially speed things up
getTimezone(lon, lat, countryCodes = countryCodes)
```

```
## [1] "America/Los_Angeles" "America/New_York"    "Europe/Warsaw"      
## [4] "Europe/Paris"
```

```r
getTimezone(lon, lat, allData=TRUE, countryCodes = countryCodes)
```

```
##              timezone UTC_offset UTC_DST_offset countryCode   longitude
## 1 America/Los_Angeles         -8             -7          US -118.242778
## 2    America/New_York         -5             -4          US  -74.006389
## 3       Europe/Warsaw          1              2          PL   21.000000
## 4        Europe/Paris          1              2          FR    2.333333
##   latitude
## 1 34.05083
## 2 40.71167
## 3 52.25417
## 4 48.88111
```

## `getUSCounty()`

Returns the US County which name pairs of coordinates fall in. The arguments are similar 
as above except that `stateCodes=c()` is used instead of `countryCodes=c()` since this 
dataset is US specific. 


```r
# Load counties dataset if you haven't already
loadSpatialData("USCensusCounties")

# New dataset of points only in the US
stateCodes <- getStateCode(lon,lat)

# Optionally pass the state codes as an argument to speed everything up
getUSCounty(lon, lat, stateCodes=stateCodes)

getUSCounty(lon, lat, allData=TRUE, stateCodes=stateCodes)
```

## Timezone Map

While identifying the states, countries and timezones associatated with a set of 
locations is important, we can also generate some quick eye candy with these datasets.
Let's color the timezones by the data variable 'UTC_offset'


```r
library(sp)         # For spatial plotting

# Assign timezones polygons an index based on UTC_offset
colorIndices <- .bincode(SimpleTimezones@data$UTC_offset, breaks=seq(-12.5,12.5,1))

# Color our timezones by UTC_offset
plot(SimpleTimezones, col=rainbow(25)[colorIndices])
title(line=-3,'Timezone Offsets from UTC')
```

## Working with ISO 3166-1 Encoded Data

On of the main reasons for ensuring that our spatial datasets use ISO encoding is that 
it makes it easy to generate plots with any datasets that use that encoding. Here is a 
slightly more involved example using Energy data from the British Petroleum Statistical 
Review that has been ISO-encoded.


```r
library(sp)         # For spatial plotting

# Read in ISO-encoded oil production and consumption data
prod <- read.csv(url('http://mazamascience.com/OilExport/BP_2014_oil_production_bbl.csv'),
                 skip=6, stringsAsFactors=FALSE, na.strings='na')
cons <- read.csv(url('http://mazamascience.com/OilExport/BP_2014_oil_consumption_bbl.csv'),
                 skip=6, stringsAsFactors=FALSE, na.strings='na')

# Only work with ISO-encoded columns of data
prodCountryCodes <- names(prod)[ stringr::str_length(names(prod)) == 2 ]
consCountryCodes <- names(cons)[ stringr::str_length(names(cons)) == 2 ]

# Use the last row (most recent data)
lastRow <- nrow(prod)
year <- prod$YEAR[lastRow]

# Neither dataframe contains all countries so create four categories based on the
# amount of information we have:  netExporters, netImporters, exportOnly, importOnly
sharedCountryCodes <- intersect(prodCountryCodes,consCountryCodes)
net <- prod[lastRow, sharedCountryCodes] - cons[lastRow, sharedCountryCodes]

# Find codes associated with each category
netExportCodes <- sharedCountryCodes[net > 0]
netImportCodes <- sharedCountryCodes[net <= 0]
exportOnlyCodes <- setdiff(prodCountryCodes,consCountryCodes)
importOnlyCodes <- setdiff(consCountryCodes,prodCountryCodes)

# Create a logical 'mask' associated with each category
netExportMask <- SimpleCountries@data$countryCode %in% netExportCodes
netImportMask <- SimpleCountries@data$countryCode %in% netImportCodes
onlyExportMask <- SimpleCountries@data$countryCode %in% exportOnlyCodes
onlyImportMask <- SimpleCountries@data$countryCode %in% importOnlyCodes

color_export = '#40CC90'
color_import = '#EE5555'
color_missing = 'gray90'

# Base plot (without Antarctica)
notAQ <- SimpleCountries@data$countryCode != 'AQ'
plot(SimpleCountries[notAQ,],col=color_missing)

plot(SimpleCountries[netExportMask,],col=color_export,add=TRUE)
plot(SimpleCountries[onlyExportMask,],col=color_export,add=TRUE)
plot(SimpleCountries[netImportMask,],col=color_import,add=TRUE)
plot(SimpleCountries[onlyImportMask,],col=color_import,add=TRUE)

legend('bottomleft',legend=c('Net Exporters','Net Importers'),fill=c(color_export,color_import))
title(line=0,paste('World Crude Oil in',year))
```
