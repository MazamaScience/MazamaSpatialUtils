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

 * new conversion script for smoke data from the [NOAA Hazard Mapping Service](http://www.ospo.noaa.gov/Products/land/hms.html)

### MazamaSpatialUtils 0.4.2

 * added `encoding` argument to `convertLayer()`
 * modified `convertUSCensusCounties() to use `encoding='latin'`

## MazamaSpatialUtils 0.4.1

 * added `useBuffering` arguent to `getState()`, `getCountry()` and `getTimezone()`

