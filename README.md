
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/MazamaSpatialUtils)](https://cran.r-project.org/package=MazamaSpatialUtils)
[![Downloads](https://cranlogs.r-pkg.org/badges/MazamaSpatialUtils)](https://cran.r-project.org/package=MazamaSpatialUtils)
[![DOI](https://zenodo.org/badge/46367920.svg)](https://zenodo.org/badge/latestdoi/46367920)

A dedicated Slack channel has been created for announcements, support and to help build a community of practice around this open source package. You may request an invitation to join from jonathan.callahan@dri.com.

# MazamaSpatialUtils

```
A suite of conversion functions to create internally standardized
spatial polygons data frames. Utility functions use these data sets to
return values such as country, state, time zone, watershed, etc. associated
with a set of longitude/latitude pairs. (They also make cool maps.)
```

## Background

The **MazamaSpatialUtils** package was created to regularize 
work with spatial data. Many sources of shapefile
data are available and can be used to make beautiful maps in R. Unfortunately,
the data attached to these datasets, even when fairly complete, often lacks
standardized identifiers such as the ISO 3166-1 alpha-2 encodings for countries.
Maddeningly, even when these ISO codes are used, the dataframe column in which
they are stored does not have a standardized name. It may be called "ISO" or "ISO2"
or "alpha" or "COUNTRY" or any of a dozen other names we have seen.

While many mapping packages provide _"natural"_ naming of countries, those who
wish to develop operational, GIS-like systems need something that is both
standardized and language-independent. The ISO 3166-1 alpha-2 encodings have
emerged as the _de facto_ standard for this sort of work. In similar fashion, ISO
3166-2 alpha-2 encodings are available for the next administrative level down --
state/province/oblast, _etc._ For time zones, the _de facto_ standard is the set of
Olson time zones used in all UNIX systems.

The main goal of this package is to create an internally standardized set of
spatial data that can be used in various projects. Along with three built-in
datasets, this package provides `convert~()` functions for other spatial datasets
of interest. These convert functions all follow the same recipe:

 * download spatial data into a standard directory
 * convert spatial data into a **sf** simple features data frame
 * modify the dataframe so that it adheres to package internal standards

Other datasets can be added following the same procedure.

The 'package internal standards' are very simple.

1) Every spatial dataset **must** contain the following columns:

* polygonID -- unique identifier for each polygon
* countryCode -- country at centroid of polygon (ISO 3166-1 alpha-2)

2) Spatial datasets with time zone data **must** contain the following column:

* timezone -- Olson timezone

3) Spatial datasets at scales smaller than the nation-state **should** contain the following column:

* stateCode -- 'state' at centroid of polygon (ISO 3166-2 alpha-2)

If other columns contain these data, those columns must be renamed or duplicated with the 
internally standardized name. This simple level of consistency makes it possible to generate 
maps for any data that is ISO encoded. It also makes it possible to create functions that 
return the country, state or time zone associated with a set of locations.

## Installation

This package is designed to be used with [R](https://cran.r-project.org) (>= 4.0)
and [RStudio](https://posit.co/) so make sure you have those installed first.

Installation from CRAN is standard:

```
install.packages("MazamaSpatialUtils")
```

Or you can use the **devtools** package to install the latest version from GitHub:

```
devtools::install_github('mazamascience/MazamaSpatialUtils', build_vignettes=TRUE)
```

## Spatial Datasets

### Package Datasets

The package comes with the following simplified spatial spatial datasets:

```
 * 367K	data/SimpleCountries.rda
 * 1.3M	data/SimpleCountriesEEZ.rda
 * 1.4M	data/SimpleTimezones.rda
```

These datasets allow you to work with low-resolution country outlines and
time zones.

### Additional Datasets

Additional datasets are available at 
http://data.mazamascience.com/MazamaSpatialUtils/Spatial_0.8/
and can be loaded with the following commands:

```
# Create a location where spatial datasets will be stored
dir.create('~/Data/Spatial_0.8', recursive = TRUE)

# Tell the package about this location
setSpatialDataDir('~/Data/Spatial_0.8')

# Install spatial datasets by name
installSpatialData("EPARegions")
```

You can review your currently install**ed** datasets by running:

```
installedSpatialData()
```

Further details about each dataset are provided in the source code of
the associated `convert~()` 
function. Datasets appearing with, *e.g.*, `_05` are simplified datasets whose 
polygons retain only 5% of the vertices in the full resolution dataset.


## Examples

### Demos

There are three demos associated with the package:

```
demo(package = 'MazamaSpatialUtils')
```

----

Development of this R package has been supported with funding from the 
following institutions:

* USFS [AirFire Research Team](https://www.airfire.org)

Questions regarding further development of the package or inclusion of additional
datasets should be directed to <jonathan.callahan@dri.edu>.


