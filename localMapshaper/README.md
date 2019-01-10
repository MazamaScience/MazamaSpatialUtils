# Mapshaper

Mapshaper is a tool for editing spatial data, written in javascript. We mainly use it to simplify polygons. It has a web interface, found at [mapshaper.org](mapshaper.org). It can also be downloaded locally, providing use of its command line interface. Full documentation can be found on [github](https://github.com/mbloch/mapshaper). 

## Download
Mapshaper requires Nodejs. If you don't have it, you can install it [here](https://nodejs.org/en/). 

With node, you can install the latest version using npm:

```
npm install -g mapshaper
```

That's all. 

## Editing spatial data on the command line

[Introduction to the command line tool](https://github.com/mbloch/mapshaper/wiki/Introduction-to-the-Command-Line-Tool)

Full documentation for the command-line interface can be found [here](https://github.com/mbloch/mapshaper/wiki/Command-Reference)

Mapshaper can be used on the following file formats:  
  * Shapefile  
  * GeoJSON  
  * TopoJSON  
  * DBF   
  * delimited (CSV)  

If your file is in some other format, you can use `ogr2ogr` (which comes with `gdal`) to convert it. Note that converting to a shapefile will automatically edit field names to be no longer than 10 characters. 

### Examples

```
# Read a Shapefile, simplify using Douglas-Peucker, output as GeoJSON
mapshaper provinces.shp -simplify dp 20% -o format=geojson out.json
```

#### WBDHUC

Convert to geojson
```
ogr2ogr WBDHU2.geojson WBD.gdb -f "GeoJSON" -skipfailures WBDHU2
```

Simplify to 2%
```
mapshaper WBDHUC2.geojson -simplify 2% -o WBDHU2_02.geojson
```
