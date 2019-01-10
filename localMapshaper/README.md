# Mapshaper

Mapshaper is a tool for editing spatial data, written in javascript. We mainly use it to simplify polygons. It has a web interface, found at [mapshaper.org](mapshaper.org). It can also be downloaded locally, providing use of its command line interface. Full documentation can be found on [github](https://github.com/mbloch/mapshaper). 

## Download
Mapshaper requires Nodejs. If you don't have it, you can install it [here](https://nodejs.org/en/). 

With node, you can install the latest version using npm:

```
npm install -g mapshaper
```

That's all. 

## Mapshaper GUI
The mapshaper download comes with a GUI. You can run it with 

```
mapshaper-gui
```

This will open a browser window, which you can add files to and perform actions like simplification. 

You can also access the gui online at [mapshaper.org](mapshaper.org).

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

WBDHUC files are downloaded as .gdb from the [USGS](https://prd-tnm.s3.amazonaws.com/index.html?prefix=StagedProducts/Hydrography/WBD/National/GDB). They must be converted into something that can be read by mapshaper. The most efficient option is to convert them to Shapefiles. Note that converting to a shapefile will automatically edit field names to be no longer than 10 characters, and truncate values that are longer than 254 characters. Another option would be to convert to geojson. This will preserve the original data but will take much longer and generate much larger files, and can only be done one layer at a time. 

Convert to shapefile
```
ogr2ogr WBD WBD.gdb -f "ESRI Shapefile"
```

Convert WBDHU2 to geojson
```
ogr2ogr WBDHU2.geojson WBD.gdb -f "GeoJSON" -skipfailures WBDHU2
```

Simplify to 2%
```
mapshaper WBDHUC2.shp -simplify 2% -o WBDHU2_02.shp
```

The Makefile in this directory has targets to download the WBD geodatabase file, unzip it, convert it to a shapefile, and simplify all the HUC layers in the shapefile to 1% and 2%. By default, all data is downloaded to `~/Data/Spatial` but you can override this with `SPATIAL_DATA_DIR`. 

#### Download and unzip geodatabase file to `~/Data`
```
make SPATIAL_DATA_DIR=~/Data download_wbd
```

#### Convert geodatabase file to shapefile  
*You must already have `WBD_National_GDB.gdb` downloaded to `SPATIAL_DATA_DIR`*

```
make SPATIAL_DATA_DIR=~/Data convert_to_shapefile
```

#### Simplify all HUC layers

```
make SPATIAL_DATA_DIR=~/Data simplify_all
```

### Simplify only WBDHUC2

```
make SPATIAL_DATA_DIR=~/Data simplify_hu2
```

