<!-- [![Build Status](https://travis-ci.org/mazamascience/MazamaSpatialUtils.svg)](https://travis-ci.org/mazamascience/MazamaSpatialUtils)
[![Coverage Status](https://coveralls.io/repos/mazamascience/MazamaSpatialUtils/badge.svg?branch=master&service=github)](https://coveralls.io/github/mazamascience/MazamaSpatialUtils?branch=master) -->
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/MazamaSpatialUtils)](https://cran.r-project.org/package=MazamaSpatialUtils)
[![Downloads](http://cranlogs.r-pkg.org/badges/MazamaSpatialUtils)](https://cran.r-project.org/package=MazamaSpatialUtils)

# MazamaSpatialUtils

```A suite of conversion scripts to create internally standardized spatial
polygons dataframes. Utility scripts use these datasets to return values such
as country, state, timezone, watershed, etc. associated with a set of 
longitude/latitude pairs. (They also make cool maps.)```

## Background

The MazamaSpatialUtils package was created by MazamaScience to regularize our
work with spatial data. The sp, rgdal and maptools packages have made it much
easier to work with spatial data found in shapefiles. Many sources of shapefile
data are available and can be used to make beautfiul maps in R. Unfortunately,
the data attached to these datasets, even when fairly complete, often lacks
standardized identifiers such as the ISO 3166-1 alpha-2 encodings for countries.
Maddeningly, even when these ISO codes are used, the dataframe column in which
they are stored does not have a standardized name. It may be called ISO or ISO2
or alpha or COUNTRY or any of a dozen other names we have seen.

While many mapping packages provide ‘natural’ naming of countries, those who
wish to develop operational, GIS-like systems need something that is both
standardized and language-independent. The ISO 3166-1 alpha-2 encodings have
emerged as the defacto standard for this sort of work. In similar fashion, ISO
3166-2 alpha-2 encodings are aviailable for the next administrative level down –
state/province/oblast, etc.. For timezones, the defacto standard is the set of
Olson timezones used in all UNIX systems.

The main goal of this package is to create an internally standardized set of
spatial data that we can use in various projects. Along with two built-in
datasets, this package provides ‘convert~’ functions for other spatial datasets
that we currently use. These convert functions all follow the same recipe:

 * download spatial data in shapefile format into a standard directory
 * convert shapefile data into a sp SpatialPolygonsDataFrame
 * modify the dataframe in the @data slot so that it adheres to package internal standards

Other datasets can be added following the same procedure.

The ‘package internal standards’ are very simple. Every spatial dataset will
have at least one of the following, consistently named colums of data:

 * countryCode – ISO 3166-1 alpha-2
 * stateCode – ISO 3166-2 alpha-2
 * timezone – Olson timezone

If another column contains this data, that column must be renamed or
duplicated with the internally standardized name. This simple level of
consistency makes it posisble to generate maps for any data that is ISO encoded.
It also makes it possible to create functions that return the country, state or
timezone associated with a set of locations.


## Installation

This package is designed to be used with [R](https://cran.r-project.org) (>= 3.1.0)
and [RStudio](http://rstudio.com) so make sure you have those installed first.

Users will want to install the **devtools** package to have access to latest versions 
of some packages that are not yet available on CRAN.

The following packages should be installed with devtools by typing the following at the RStudio console:

``` devtools::install_github('mazamascience/MazamaSpatialUtils', build_vignettes=TRUE) ```

## Data

Pre-generated .RData versions of the following standardized shapefiles are available:

 * NaturalEarthAdm1.RData	15M	 
 * TMWorldBorders.RData	3.6M	 
 * USCensusCounties.RData	2.2M	 
 * WBDHU2.RData	1.2M	 
 * WBDHU4.RData	3.3M	 
 * WBDHU6.RData	4.0M	 
 * WBDHU8.RData	7.2M	 
 * WBDHU10.RData 21M	 
 * WBDHU12.RData 48M	 
 * WorldTimezones.RData	16M

To get these datasets you should execute the following commands from a shell terminal:

```
mkdir ~/Data
cd ~/Data
curl -O http://mazamascience.com/RData/Spatial.tar.gz
tar -xzf Spatial.tar.gz
```

## Examples

The package vigentte 'Introduction to MazamaSpatialUtils' has numerous examples.


----

This project is supported by Mazama Science.


