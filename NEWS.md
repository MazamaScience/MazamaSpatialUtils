# Updates to the MazamaSpatialUtils R Package

```
A suite of conversion scripts to create internally standardized
spatial polygons dataframes. Utility scripts use these datasets to return
values such as country, state, timezone, watershed, etc. associated with a
set of longitude/latitude pairs. (They also make cool maps.)
```

----

## Version 0.5 -- Aggregate by Polygon

### MazamaSpatialUtils 0.5.0

 * package now `Depends` on `sp` package so that `sp` plotting is used by default
 * package includes higher resolution `SimpleCountries` and `SimpleTimezones` datasets
 * internal standard now requires unique `polygonID` column for every dataset
 * organizePolygons() now uniformly uses `polygonID` for rownames and polygons@ID so you can say:
 `plot(SimpleCountries['IT',])`
 * fixed bug in processing of `USCensusCounties` dataset
 * changed codeToCode() to two functions: iso2ToIso3() and iso3ToIso2()
 * package now includes SimpleCountriesEEZ dataset which is used as the default dataset for getCountry.
 SimpleCountriesEEZ includes a 200-mile offshore buffer for more efficient and accurate spatial searches.

## Version 0.4 -- Fire and Water

### MazamaSpatialUtils 0.4.9

 * new .tar.gz file available containing spatial datasets
 * new installSpatialData() installs all required datasets
 * loadSpatialData() no loads one or more datasets based on a pattern
 * removed initializeSpatialData()
 * convertGADM() updated to GADM version 2.8 (handles .rds files)
 * updated localVignettes/ebolaMap.Rmd
 * new docker/ directory for building docker images to run MazamaSpatialUtils
 * new app/ directory demonstrates dockerized web-service based on MazamaSpatialUtils

### MazamaSpatialUtils 0.4.8

 * Locations that do not intersect any polygon now return `NA` rather than generating warnings.
 * Various minor bug fixes.

### MazamaSpatialUtils 0.4.5

 * `convertHMSSmoke()` now handles shapefiles with now `Density` information.

### MazamaSpatialUtils 0.4.4

 * Fixed bug/typo in `findTimezones` demo.
 * Fixed bugs/typos in `convertWBDHUC()` and `convertHMSSmoke()`.

### MazamaSpatialUtils 0.4.3

 * New conversion script for smoke data from the [NOAA Hazard Mapping Service](http://www.ospo.noaa.gov/Products/land/hms.html).
 * Shapefiles with no projection information are assigned `"+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"`.
 * Added dependency on `lubridate` package,

### MazamaSpatialUtils 0.4.2

 * Added `encoding` argument to `convertLayer()`.
 * Modified `convertUSCensusCounties() to use `encoding='latin'`.

### MazamaSpatialUtils 0.4.1

 * Added `useBuffering` arguent to `getState()`, `getCountry()` and `getTimezone()`.

