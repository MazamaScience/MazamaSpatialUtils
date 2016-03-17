# Updates to the MazamaSpatialUtils R Package

```
A suite of conversion scripts to create internally standardized
spatial polygons dataframes. Utility scripts use these datasets to return
values such as country, state, timezone, watershed, etc. associated with a
set of longitude/latitude pairs. (They also make cool maps.)
```

----

## Version 0.4 -- Fire and Water

### MazamaSpatialUtils 0.4.3

 * Fixed bugs/typos in `convertWBDHUC()` and `convertHMSSmoke()`.

### MazamaSpatialUtils 0.4.3

 * New conversion script for smoke data from the [NOAA Hazard Mapping Service](http://www.ospo.noaa.gov/Products/land/hms.html).
 * Shapefiles with no projection information are assigned `"+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"`.
 * Added dependency on `lubridate` package,

### MazamaSpatialUtils 0.4.2

 * Added `encoding` argument to `convertLayer()`.
 * Modified `convertUSCensusCounties() to use `encoding='latin'`.

## MazamaSpatialUtils 0.4.1

 * Added `useBuffering` arguent to `getState()`, `getCountry()` and `getTimezone()`.

