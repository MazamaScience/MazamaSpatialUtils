# Web Service App #

This directory contains code to set up a simple web service based on the functionality
provided by the **MazamaSpatialUtils** package. It is built on top of the **jug** package

From https://github.com/Bart6114/jug:

> Jug is a small web development framework for R which relies heavily upon the `httpuv` package.
> It’s main focus is to make > building APIs for your code as easy as possible.
> 
> Jug is not supposed to be either an especially performant nor an uber stable web framework.
> Other tools (and languages) might be more suited for that. It focuses on maximizing the ease
> with wich you can create web APIs for your R code. However, the flexibility of Jug means that,
> in theory, you could built an extensive web framework with it.

The `spatialUtilsApp.R` file uses **jug** (similar to python [Flask](http://flask.pocoo.org)) to create a set of URL paths that
map onto different functionality available in the package. In this minimal example we provide functionality at the following paths:

 * `spatialUtils/getCountry`
 * `spatialUtils/getHUC2`
 * `spatialUtils/getHUC4`
 * `spatialUtils/getHUC6`
 * `spatialUtils/getHUC9`
 * `spatialUtils/getHUC10`
 * `spatialUtils/getHUC12`
 * `spatialUtils/getState`
 * `spatialUtils/getTimezone`
 
Each URL accepts arguments of the form `?lons=<R expression>&lats=<R expression>`

*(Note: Blindly evaluating expressions coming in from the web is not recommended for production systems!)*

The accompanying `Dockerfile` lets you build a self-contained webservice image that can be managed by docker.

Here is a quick exmaple of how to build and launch the webservice on localhost port 8080

```
$ docker build -t spatialutils-app:latest .
...
$ docker images
REPOSITORY                   TAG                 IMAGE ID            CREATED             SIZE
spatialutils-app             latest              e55766f25a2d        4 seconds ago       1.21 GB
...
$ docker run --rm -p 8080:8080 spatialutils-app
Loading required package: magrittr

Attaching package: ‘jug’

The following object is masked from ‘package:base’:

    get

Serving the jug at http://0.0.0.0:8080
```

Now you can try the following URLs in your browser:

http://localhost:8080/spatialUtils/getCountry?lons=seq(10,50,10)&lats=seq(10,50,10)

```
longitude,latitude,countryName,countryCode,ISO3,FIPS
10.0,10.0,Nigeria,NG,NGA,NI
20.0,20.0,Chad,TD,TCD,CD
30.0,30.0,Egypt,EG,EGY,EG
40.0,40.0,Turkey,TR,TUR,TU
50.0,50.0,Kazakhstan,KZ,KAZ,KZ
```

http://localhost:8080/spatialUtils/getState?lons=seq(10,50,10)&lats=seq(10,50,10)

```
longitude,latitude,countryName,stateName,countryCode,stateCode,FIPS,languageCode
10.0,10.0,Nigeria,Bauchi,NG,BA,NI46,fra
20.0,20.0,Chad,Borkou,TD,BR,CD23,fra
30.0,30.0,Egypt,Matruh,EG,MT,EG22,ara
40.0,40.0,Turkey,Erzincan,TR,EN,TU24,tur
50.0,50.0,Kazakhstan,West Kazakhstan,KZ,WK,KZ07,rus
```

http://localhost:8080/spatialUtils/getTimezone?lons=seq(10,50,10)&lats=seq(10,50,10)

```
longitude,latitude,timezone,countryCode,UTC_offset,UTC_DST_offset
10.0,10.0,Africa/Lagos,NG,1.0,1.0
20.0,20.0,Africa/Ndjamena,TD,1.0,1.0
30.0,30.0,Africa/Cairo,EG,2.0,3.0
40.0,40.0,Europe/Istanbul,TR,2.0,3.0
50.0,50.0,Asia/Oral,KZ,5.0,5.0
```
