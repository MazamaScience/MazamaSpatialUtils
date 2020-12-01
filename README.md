---
title: "MazamaSpatialUtils"
pagetitle: MazamaSpatialUtils
---

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/MazamaSpatialUtils)](https://cran.r-project.org/package=MazamaSpatialUtils)
[![Downloads](http://cranlogs.r-pkg.org/badges/MazamaSpatialUtils)](https://cran.r-project.org/package=MazamaSpatialUtils)
[![Build Status](https://travis-ci.org/MazamaScience/MazamaSpatialUtils.svg?branch=master)](https://travis-ci.org/MazamaScience/MazamaSpatialUtils)

# MazamaSpatialUtils

```
A suite of conversion functions to create internally standardized
spatial polygons data frames. Utility functions use these data sets to
return values such as country, state, timezone, watershed, etc. associated
with a set of longitude/latitude pairs. (They also make cool maps.)
```

## Background

The MazamaSpatialUtils package was created by MazamaScience to regularize our
work with spatial data. The sp, rgdal and maptools packages have made it much
easier to work with spatial data found in shapefiles. Many sources of shapefile
data are available and can be used to make beautiful maps in R. Unfortunately,
the data attached to these datasets, even when fairly complete, often lacks
standardized identifiers such as the ISO 3166-1 alpha-2 encodings for countries.
Maddeningly, even when these ISO codes are used, the dataframe column in which
they are stored does not have a standardized name. It may be called ISO or ISO2
or alpha or COUNTRY or any of a dozen other names we have seen.

While many mapping packages provide ‘natural’ naming of countries, those who
wish to develop operational, GIS-like systems need something that is both
standardized and language-independent. The ISO 3166-1 alpha-2 encodings have
emerged as the defacto standard for this sort of work. In similar fashion, ISO
3166-2 alpha-2 encodings are available for the next administrative level down –
state/province/oblast, etc.. For timezones, the defacto standard is the set of
Olson timezones used in all UNIX systems.

The main goal of this package is to create an internally standardized set of
spatial data that we can use in various projects. Along with three built-in
datasets, this package provides ‘convert~’ functions for other spatial datasets
that we currently use. These convert functions all follow the same recipe:

 * download spatial data in shapefile format into a standard directory
 * convert shapefile data into a sp SpatialPolygonsDataFrame
 * modify the dataframe in the `@data` slot so that it adheres to package internal standards

Other datasets can be added following the same procedure.

The 'package internal standards' are very simple.

1) Every spatial dataset **must** contain the following columns:

* polygonID -- unique identifier for each polygon
* countryCode -- country at centroid of polygon (ISO 3166-1 alpha-2)

2) Spatial datasets with timezone data **must** contain the following column:

* timezone -- Olson timezone

3) Spatial datasets at scales smaller than the nation-state **should** contain the following column:

* stateCode -- 'state' at centroid of polygon (ISO 3166-2 alpha-2)

If other columns contain these data, those columns must be renamed or duplicated with the 
internally standardized name. This simple level of consistency makes it possible to generate 
maps for any data that is ISO encoded. It also makes it possible to create functions that 
return the country, state or timezone associated with a set of locations.

## Installation

This package is designed to be used with [R](https://cran.r-project.org) (>= 3.1.0)
and [RStudio](https://rstudio.com/) so make sure you have those installed first.

Users can use the **devtools** package to install the latest version of the 
package which may have new features that are not yet available on CRAN:

```
devtools::install_github('mazamascience/MazamaSpatialUtils', build_vignettes=TRUE)
```

## Spatial Datasets

### Package Datasets

The package comes with the following simplified spatial spatial datasets:

```
 * 276K	data/SimpleCountries.RData
 * 2.1M	data/SimpleCountriesEEZ.RData
 * 1.1M	data/SimpleTimezones.RData
```

These datasets allow you to work with low-resolution country outlines and
timezones.

### Core Datasets

Additional datasets are available at 
http://data.mazamascience.com/MazamaSpatialUtils/Spatial/
and can be loaded with the following commands:

```
# Create a location where large spatial datasets will be stored
dir.create('~/Data/Spatial', recursive = TRUE)

# Tell the package about this location
setSpatialDataDir('~/Data/Spatial')

# Install core spatial data
installSpatialData()
```

Datasets included in the core set include:

```
 * 2.1M EEZCountries.RData
 *  15M NaturalEarthAdm1.RData
 *  61M OSMTimezones.RData
 * 3.0M	OSMTimezones_05.RData
 * 3.6M TMWorldBorders.RData
 *  48MTerrestrialEcoregions.RData
 * 3.5M TerrestrialEcoregions_05.RData
 * 7.5M USCensus115thCongress.RData
 *  17M USCensusCounties.RData
 * 4.6M USCensusStates.RData
 * 1.2M USIndianLands.RData
 *  17M WorldTimezones.RData
```

Further details about each dataset are provided in the associated `convert~()` 
function. Datasets appearing with, *e.g.*, `_05` are simplified datasets whose 
polygons retain only 5% of the vertices of the original .

### Additional Datasets

Mazama Science regularly generates new datasets that adhere to package standards.
These can be download manually from 
http://data.mazamascience.com/MazamaSpatialUtils/Spatial/. As
of Jan 10, 2019, the full list of available datasets includes:

```
 * 24K	CA_AirBasins_01.RData
 * 44K	CA_AirBasins_02.RData
 * 100K	CA_AirBasins_05.RData
 * 2.1M	CA_AirBasins.RData
 * 2.2M	EEZCountries.RData
 * 404K	GACC_05.RData
 * 7.0M	GACC.RData
 * 15M	NaturalEarthAdm1.RData
 * 3.1M	OSMTimezones_05.RData
 * 62M	OSMTimezones.RData
 * 3.6M	TerrestrialEcoregions_05.RData
 * 49M	TerrestrialEcoregions.RData
 * 3.7M	TMWorldBorders.RData
 * 7.6M	USCensus115thCongress.RData
 * 564K	USCensusCBSA_01.RData
 * 944K	USCensusCBSA_02.RData
 * 2.0M	USCensusCBSA_05.RData
 * 34M	USCensusCBSA.RData
 * 2.3M	USCensusCounties.RData
 * 3.5M	USCensusStates.RData
 * 1.2M	USIndianLands.RData
 * 769M	WBDHU10.RData
 * 1.5G	WBDHU12.RData
 * 424K	WBDHU2_01.RData
 * 840K	WBDHU2_02.RData
 * 38M	WBDHU2.RData
 * 1.1M	WBDHU4_01.RData
 * 2.2M	WBDHU4_02.RData
 * 108M	WBDHU4.RData
 * 1.4M	WBDHU6_01.RData
 * 2.8M	WBDHU6_02.RData
 * 137M	WBDHU6.RData
 * 295M	WBDHU8.RData
 * 18M	WorldTimezones.RData
```

## Examples

### Vignettes

The package vignette 'Introduction to MazamaSpatialUtils' has numerous examples.

### Demos

There are three demos associated with the package:

```
demo(package = 'MazamaSpatialUtils')
```

### Shiny App

There is also an exampe R Shiny app which uses the `WBDHU#` datasets combines 
two large datasets:

 * location data from the National Bridge Inventory
 * shapefiles from the Watershed Boundary Dataset
 
The app allows you to aggregate point location data by watershed to create
summary values associated with each watershed. It also demonstrates the need
to enable caching in a shiny app when plots take a long time to generate.

```
library(MazamaSpatialUtils)
setSpatialDataDir('~/Data/Spatial')
runExample()
```

### Mapshaper

Instructions for installing the javascript `mapshaper` utility and using it to
simplify large shapefiles are found in the `localMapshaper/` directory.

----

This project is supported by Mazama Science.


