# MazamaSpatialUtils 0.8.7

Added a new set of country code conversion functions whose names match those
for state and county conversions:

- `countryCodeToName()`
- `countryCodeToFIPS()`
- `countryFIPSToName()`
- `countryCodeToCode()`
- `countryNameToCode()`
- `countryNameToFIPS()`

These are now preferred over the older `codeToCountry()` and `countryToCode()`
functions which, while deprecated, are still supported for backwards compatibility.

# MazamaSpatialUtils 0.8.6

* Addressed CRAN package documentation issue.
* Package-internal datasets can be loaded by `getCountryCode()`
and `getTimezone()` without invoking `library MazamaSpatialUtils`.

# MazamaSpatialUtils 0.8.5

* Tweaks to satisfy CRAN submission checks.

# MazamaSpatialUtils 0.8.4

* Added dependency on **sf**.

This addresses the following error seen when using internal data in a **dplyr** context:

```
> IS <- SimpleCountriesEEZ %>% dplyr::filter(countryCode == "IS")
Error in `vec_size()`:
! `x` must be a vector, not a <sfc_MULTIPOLYGON/sfc> object.
```

This error is explained here:
<https://www.mm218.dev/posts/2022-12-01-sf-in-packages/> and the simplest 
solution for this spatially-oriented package is to simply move **sf** from an
_import_ to a _dependency_ (by moving it to the "Depends:" section of `DESCRIPTION`).

# MazamaSpatialUtils 0.8.3

* Added handling of "Sparse geometry binary predicate list of length 1" error in 
`getSpatialData()` by simply choosing the first polygon returned.
* Added `convertWBDHUC()` and pre-generated spatial data files: `WBDHU2`, 
`WBDHU4`, `WBDHU6`, `WBDHU8` and `WBDHU10`.
* Added `getHUC()` and `getHUCName()`.

Added the following datasets to the archive at: 
http://data.mazamascience.com/MazamaSpatialUtils/Spatial_0.8/
* `WBDHU2` (and simplified versions)
* `WBDHU4` (and simplified versions)
* `WBDHU6` (and simplified versions)
* `WBDHU8`
* `WBDHU10`

# MazamaSpatialUtils 0.8.2

* Documentation fixes associated with change from **sp** to **sf**.

# MazamaSpatialUtils 0.8.1

* Tweaks to satisfy CRAN submission checks.

# MazamaSpatialUtils 0.8.0

Version 0.8.x is a complete refactoring of **MazamaSpatialUtils** to upgrade
from dependence on the **sp** package to use of the **sf** package. As much as
possible, the suite of functions and arguments will remain the same.

The following changes have been made:

* The `dataset` parameter has been renamed to `datasetName` in all functions
where a name is expected rather than a sf dataframe object. This should have no
effect on existing code which should "fuzzy match" `dataset => datasetName`.

# MazamaSpatialUtils 0.7.6

* Removed links to slow-to-respond http://www.naturalearthdata.com/ to pass CRAN 
submission tests.
* Updated out-of-date URLs.

# MazamaSpatialUtils 0.7.5

* Updated `ebolamap.Rmd` vignette to pass R CMD check.

# MazamaSpatialUtils 0.7.4

* Minor documentation improvements.
* Removed unused dependency on *tidyr*.
* Replaced non-ASCII characters to satisfy CRAN checks.
* Consistent parameter validation in all `get~()` functions.

# MazamaSpatialUtils 0.7.3

* URL corrections for CRAN submission. 
* Vignette wordsmithing.
* Test updates.

# MazamaSpatialUtils 0.7.2

* URL corrections after testing with win-builder.

# MazamaSpatialUtils 0.7.1

* Reordered parameters in `installSpatialData()` so that `dataset` comes first.
* `installSpatialData()` attempts to install simplified versions of datasets:
"_05", "_02" and "_01".
* Replacement of `lon` and `lat` in `getHUC()` and `getHUCName()`.
* `subsetHUC()` now handles `NA` values in `SFDF$allStateCodes`.
* Congressional districts dataset name now includes session number:  
`USCensus116thCongress`.

# MazamaSpatialUtils 0.7.0

Version 0.7 includes more datasets that have all been through identical
processing and harmonization steps. In general, this release represents a
clean-and-update revision that brings all aspects of the package up to modern
standards.

Improvements include:

* Fewer package dependencies.
* Minor updates to vignettes and articles.
* Consistent replacement of `lon` and `lat with `longitude` and `latitude` in
all functions.

# MazamaSpatialUtils 0.6.16

* Updated `convertWikipediaTimezeonTable()`.
* Updated `convertWorldTimezones()` and package internal dataset `SimpleTimezones`.
* Updated `convertGADM()` to support GADM version 3.6.
* Updated `convertEEZCountries()` and `SimpleCountriesEEZ` dataset.
* Updated `SimpleCountries` dataset

# MazamaSpatialUtils 0.6.15

* Added `US_countyCodes` dataset with `stateCode`, `stateFIPS`, `countyName`,
`countyFIPS`.

New functions for converting between US county names/FIPS:
 * `US_countyNameToFIPS()`
 * `US_countyFIPSToName()`

# MazamaSpatialUtils 0.6.14

* Fixed a bug in `loadSpatialData()` that was returning dataset names with `".rda"`.

# MazamaSpatialUtils 0.6.13

* Updated `convertWorldEEZ.R`.
* Updated `convertHMSSmoke.R`.
* Updated `convertStateLegislativeDistricts.R`.
* Updated `convertTMWorldBordersSimple.R`.
* Updated `convertTMWorldBorders.R`.
* Updated `convertSimpleCountries.R`.
* Updated `convertGACC.R`.
* Updated `convertNWSFireZones.R`.

# MazamaSpatialUtils 0.6.12

* Updated `convertMTBSBurnArea.R`.
* Updated `convertNaturalEarthAdm1.R`.

# MazamaSpatialUtils 0.6.11

* Removed non-working `app/` directory and dependency on **shiny**.
* Corrected Bosnia country code in `convertWikipediaTimezoneTable.R`.
* Updated `convertEPARegions.R`.
* Updated `convertOSMTimeZones.R`.
* Updated `convertTerrestrialEcoregions.R`.
* Updated `convertWeatherZones.R`.

# MazamaSpatialUtils 0.6.10

* Updated `convertGACC.R` to use 2020 data.
* Removed outlying territories from `US_stateCodes`.
* Updated `convertUSCensusStates.R` to use 2019 data.
* Updated `convertCARBAirBains.R` to latest coding style.
* Updated `convertUSCensusCBSA.R` to latest coding style.
* Updated `convertIndianLands.R` to latest coding style.
* Now using the *cleangeo* package to fix topology errors and geometry validity
issues.

New functions for converting among US state names/codes/FIPS:
 * `US_stateCodeToName()`
 * `US_stateCodeToFIPS()`
 * `US_stateFIPSToCode()`
 * `US_stateFIPSToName()`
 * `US_stateNameToCode()`
 * `US_stateNameToFIPS()`

# MazamaSpatialUtils 0.6.9

* Changed `US_stateCodes` dataset to include only `stateName`, `stateCode` and
`stateFIPS` columns. It is now more complete with codes for all states and
territories.
* `convertLayer()` now properly passes encoding to `rgdal::readOGR()`.
* Updated `convertUSCensusCounties()` to use 2019 data.

# MazamaSpatialUtils 0.6.8

* New "Developer Style Guide" article.

# MazamaSpatialUtils 0.6.7

* New "Basic GIS in R" article.

# MazamaSpatialUtils 0.6.6

* `loadSpatialData()` now recognizes both `.RData` and `.rda` files.
* Improved documentation
* New convert functions:
  - `convertEPARegions()`
  - `convertMTBSBurnArea()`
  - `convertUSCensusUrbanAreas()`
  - `convertUSFSRangerDistricts()`

# MazamaSpatialUtils 0.6.5

* New `convertHILFDFederalLands()` function.
* Added **tidyr** package to Imports.
* Updated docker image.

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

# MazamaSpatialUtils 0.5.9

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
[NOAA Hazard Mapping Service](https://www.ospo.noaa.gov/products/land/hms.html).
* Shapefiles with no projection information are assigned 
`"+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"`.
* Added dependency on **lubridate** package.

# MazamaSpatialUtils 0.4.2

* Added `encoding` argument to `converLayer()`.
* Modified `convertUSCensusCounties()` to use `encoding='latin1'`.

# MazamaSpatialUtils 0.4.1

* Added `useBuffering` argument to `getState()`, `getCountry()` and `getTimezone()`.

# MazamaSpatialUtils 0.3.2

* `getSpatialData()` no longer fails on invalid/missing locations, now returns 
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

