# MazamaSpatialUtils 0.6.4

* Modified test infrastructure to address CRAN issues.

# MazamaSpatialUtils 0.6.3

* Added `useBuffering` argument to `getUSCounties()`.
* Addressed bug in `loadSpatialData()` which errored out when a directory 
existed with the same name as the dataset trying to be loaded.
* Updated `summarizeByPolygon()` to modern dplyr syntax.
* Added `%>%` operator.
* Code style refactoring.

# MazamaSpatialUtils 0.6.1

* made `simplify()` function example `donotrun` so as to 
avoid a CRAN testing failure on fedora only -- Ugh.

# MazamaSpatialUtils 0.6.0

* changes associated with minor version bump
# MazamaSpatialUtils 0.5.10

* refactored shiny app from localShiny and added it to package in inst
* added `runExample()` to run shiny examples
* updated `README.md`

# MazamaSpatialutils 0.5.9

* New convert function for public health districts
* New convert function for GACCs
* more testing

# MazamaSpatialUtils 0.5.8

* New convert functions for weather zones
* Argument consistency with `getHUC()`
* added `dissolve()` wrapper for `rmapshaper::ms_dissolve()`

# MazamaSpatialUtils 0.5.7

* documentation with **pkgdown**
* various cleanup: spell check, reformatted NEWS.md, etc.
* added `simplify()` wrapper for `rmapshaper::ms_simplify()`

# MazamaSpatialUtils 0.5.6

* added `CONUS` and `US_52` vectors of US state codes as package data
* added `US_stateCodes` dataframe as package data

# MazamaSpatialUtils 0.5.5

* added `convertStateLegislativeDistricts()` function to download and convert
US state level legislative districts on demand
* now using **countrycode** package for all code/name conversions
* `ebolamap.Rmd` converted to vignette, moved to `vignettes/` and 
`localNotebooks/` deleted
* usages of `cat()` replaced with `message()`, `warning()`, or `error()`

# MazamaSpatialUtils 0.5.4

* corrected GADM URL in documentation for `convertGADM()`

# MazamaSpatialUtils 0.5.3

* added `convertUSCensusCBSA()` to convert Metropolitan-/Micropolitan Statistical Areas
* removed `verbose` argument from `getStateCode()` function signature

# MazamaSpatialUtils 0.5.2

* fixed use of `allStateCodes` in `WBD~` datasets
* converted `USIndianLands@data` data from `factor` to `character`
* updated `docker/Dockerfile`
* added `ebolaMap` vignette

# MazamaSpatialUtils 0.5.1

* tweaks to satisfy CRAN

# MazamaSpatialUtils 0.5.0

* package now `Depends` on **sp** package so that **sp** plotting is used by default
* package includes higher resolution `SimpleCountries` and `SimpleTimezones` datasets
* internal standard now requires unique `polygonID` column for every dataset
* `organizePolygons()` now uniformly uses `polygonID` for rownames and 
`polygons@ID` so you can say:
`plot(SimpleCountries['IT',])`
* fixed bug in processing of `USCensusCounties` dataset
* changed `codeToCode()` to two functions: `iso2ToIso3()` and `iso3ToIso2()`
* package now includes SimpleCountriesEEZ dataset which is used as the default 
dataset for `getCountry()`. `SimpleCountriesEEZ` includes a 200-mile offshore 
buffer for more efficient and accurate spatial searches
* new datasets include: `TerrestrialEcoregions`, `USCensus115thCongress`, 
`USCensusStates`, `USCensusIndianLands`

# MazamaSpatialUtils 0.4.9

* new .tar.gz file available containing spatial datasets
* new `installSpatialData()` installs all required datasets
* `loadSpatialData()` no loads one or more datasets based on a pattern
* removed `initializeSpatialData()`
* `convertGADM()` updated to GADM version 2.8 (handles .rds files)
* updated `localVignettes/ebolaMap.Rmd`
* new docker/ directory for building docker images to run **MazamaSpatialUtils**
* new app/ directory demonstrates dockerized web-service based on 
**MazamaSpatialUtils**

# MazamaSpatialUtils 0.4.8

* locations that do not intersect any polygon now return `NA` rather than 
generating warnings
* various minor bug fixes.

# MazamaSpatialUtils 0.4.5

* `convertHMSSmoke()` now handles shapefiles with new `Density` information

# MazamaSpatialUtils 0.4.4

* Fixed bug/typo in `findTimezones` demo.
* Fixed bugs/typos in `convertWBDHUC()` and `convertHMSSmoke()`.

# MazamaSpatialUtils 0.4.3

* New `convertHMSSmoke()` function for smoke data from the 
[NOAA Hazard Mapping Service](http://www.ospo.noaa.gov/Products/land/hms.html).
* Shapefiles with no projection information are assigned 
`"+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"`.
* Added dependency on **lubridate** package.

# MazamaSpatialUtils 0.4.2

* Added `encoding` argument to `converLayer()`.
* Modified `convertUSCensusCounties()` to use `encoding='latin1'`.

# MazamaSpatialUtils 0.4.1

* Added `useBuffering` arguent to `getState()`, `getCountry()` and `getTimezone()`.

# MazamaSpatialUtils 0.3.2

* `getSpatialData()` no longer fails on invliad/missing locations, now returns 
dataframe rows with all NA.

# MazamaSpatialUtils 0.3.1 -- addition of buffered search and WorldEEZ polygons

* Updated included datasets to use 
`"+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"`.
* Addition of buffered search so that locations can find nearby polygons.
* Addition of `convertWorldEEZ()` function.


# MazamaSpatialUtils 0.2.4

* Updated default projection from 
`"+proj=longlat"` to `"+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"`
to support libproj >= 4.9.1

# MazamaSpatialUtils 0.2.3

* Removed unneeded test that failed with sp version 1.1-0.

# MazamaSpatialUtils 0.2.2 -- minor tweaks to 0.2.1

* User specification of `SpatialDataDir` is now required.
* Minor documentation improvements.


# MazamaSpatialUtils 0.2.1 -- addition of GADM and USGS HUC8

* Convert functions for GADM administrative boundaries and and USGS watershed 
datasets.
* Addition of code-name, name-code and code-code conversion utilities.
* Addition of `organizePolygons()` function.

# MazamaSpatialUtils 0.1 -- initial release

