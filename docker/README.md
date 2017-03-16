# Monitoring Data Ingest #

Scripts in this repository do all the work of converting raw monitoring data from AirNow, 
AIRSIS and WRCC into .RData files ready for use in the [monitoring v3 site](http://tools.airfire.org/monitoring/v3/). 
They are designed to be run hourly in a cronjob.

## Files ##

 * `Dockerfile` -- Build instructions for the docker image used to run these scripts.
 * `README.md` -- This file.
 * `airnow_createLatestDataframes_exec.R` -- Converts recent AirNow data into 'meta' and 'data' data frames.
 * `airnow_createMonthlyDataframes_exec.R` -- Converts a given month of AirNow data into 'meta' and 'data' data frames.
 * `airsis_createLatestDataframes_exec.R` -- Converts raw AIRSIS dump files into 'meta' and 'data' data frames.
 * `createCSV_exec.R` -- Converts local .RData files into named .csv files needed by the monitoring v3 data browser.
 * `createGeoJSON_exec.R` -- Converts local .RData files into a .geojson file appropriate for display in Google maps.
 * `exampleCrontab.txt` -- Example crontab entries.
 * `py_logging.R` -- Wrapper around futile.logger package that mimics python style logging.
 * `updateTimes.py` -- Writes out a .json file with last updated times for all data files. This is used by the monitoring v3 data browser.
 * `wrcc_createLatestDataframes_exec.R` -- Converts raw WRCC dump files into 'meta' and 'data' data frames.

## Docker Setup ##

All dependencies are available in the docker image that can be built with the `Dockerfile`.

A quick refresher on docker commands is available at the [docker cheatsheet](https://github.com/wsargent/docker-cheat-sheet).

### Check Images ###

Note that not all users have permission to run `docker`. You can check existing images with:

```
$ docker images
REPOSITORY                     TAG                 IMAGE ID            CREATED             SIZE
monitoring-v3-data-ingest      latest              95634344fd74        34 minutes ago      1.26 GB
mazamascience/pwfslsmoke       latest              cc9c4c48c037        2 hours ago         1.26 GB
mazamascience/pwfslsmoke       v0.99.9             cc9c4c48c037        2 hours ago         1.26 GB
...
```

### Build Images ###

The docker image needed to run the data processing scripts can be created in this directory with:

```
$ docker build -t monitoring-v3-data-ingest:latest .
```

If the `mazamascience/pwfslsmoke` image is not available you can pull a versioned image from Docker Hub with:

```
$ docker pull mazamascience/pwfslsmoke:v0.99.9
```

You can also build a versioned image from the `Dockerfile` in the PWFSLSmoke source code `docker/` directory with:

```
$ docker build --no-cache -t mazamascience/pwfslsmoke:v0.99.9 .
$ docker tag mazamascience/pwfslsmoke:v0.99.9 mazamascience/pwfslsmoke:latest
```

> Note that the version should match the version of the `pwfslsmoke` package.

### Data Directories ###

Output directories that need to be created for installer-user `monitoring` on `haze` are:

```
/data/monitoring
/data/monitoring/RData
/data/monitoring/logs
```

These are available on the web at:  https://haze.airfire.org/monitoring/

Spatial data required by the processing scripts already exists in the docker image in `/home/mazama/data/Spatial`.

Daily monitor updates availabe on the host computer ('haze') will be linked in from their location at `/home/monitors/`.

## Docker Run ##

Having built the docker image and set up the necessary directories, we can now run the scripts. 
The following examples use `` `pwd` `` and assume that you are in the `airfire-monitoring-v3-data-ingest` directory.

### Interactive testing using R ###

```
docker run -ti -v /home/monitoring/Projects/airfire-monitoring-v3-data-ingest:/monitoring/v3 \
-v /data/monitoring:/monitoring/v3/data \
-v /home/monitors:/monitoring/v3/data/monitors \
-w /monitoring/v3 monitoring-v3-data-ingest R --vanilla
```

### AirNow ###

```
docker run -v /home/monitoring/Projects/airfire-monitoring-v3-data-ingest:/monitoring/v3 \
-v /data/monitoring:/monitoring/v3/data \
-w /monitoring/v3 monitoring-v3-data-ingest \
/monitoring/v3/airnow_createLatestDataframes_exec.R \
--outputDir=/monitoring/v3/data/RData \
--logDir=/monitoring/v3/data/logs \
--spatialDataDir=/hoe/mazama/data/Spatial
```

### AIRSIS ###

```
docker run -v /home/monitoring/Projects/airfire-monitoring-v3-data-ingest:/monitoring/v3 \
-v /data/monitoring:/monitoring/v3/data \
-v /home/monitors:/monitoring/v3/data/monitors \
-w /monitoring/v3 monitoring-v3-data-ingest \
/monitoring/v3/airsis_createLatestDataframes_exec.R \
--inputDir=/monitoring/v3/data/monitors \
--outputDir=/monitoring/v3/data/RData \
--logDir=/monitoring/v3/data/logs \
--spatialDataDir=/home/mazama/data/Spatial
```

### WRCC ###

```
docker run -v /home/monitoring/Projects/airfire-monitoring-v3-data-ingest:/monitoring/v3 \
-v /data/monitoring:/monitoring/v3/data \
-v /home/monitors:/monitoring/v3/data/monitors \
-w /monitoring/v3 monitoring-v3-data-ingest \
/monitoring/v3/wrcc_createLatestDataframes_exec.R \
--inputDir=/monitoring/v3/data/monitors/GOES_push \
--outputDir=/monitoring/v3/data/RData \
--logDir=/monitoring/v3/data/logs \
--spatialDataDir=/home/mazama/data/Spatial
```

## crontab ##

These scripts are run on an hourly basis on 'haze' with the 'monitoring' user crontab:

```
###############################################################################
# Monitoring

# m h  dom mon dow   command

# Monthly update of AirNow archival monitoring data (/home/mazama is inside the docker image)
00 01  14   *   *    docker run -v /home/monitoring/Projects/airfire-monitoring-v3-data-ingest:/monitoring/v3 -v /data/monitoring:/monitoring/v3/data -w /monitoring/v3 monitoring-v3-data-ingest /monitoring/v3/airnow_createMonthlyDataframes_exec.R --outputDir=/monitoring/v3/data/RData --logDir=/monitoring/v3/data/logs --spatialDataDir=/home/mazama/data/Spatial --yearMonth=LAST_MONTH

# Hourly update of AirNow real-time monitoring data (/home/mazama is inside the docker image)
23 *  *   *   *    docker run -v /home/monitoring/Projects/airfire-monitoring-v3-data-ingest:/monitoring/v3 -v /data/monitoring:/monitoring/v3/data -w /monitoring/v3 monitoring-v3-data-ingest /monitoring/v3/airnow_createLatestDataframes_exec.R --outputDir=/monitoring/v3/data/RData --logDir=/monitoring/v3/data/logs --spatialDataDir=/home/mazama/data/Spatial

# Hourly update of AIRSIS real-time monitoring data dump (/home/mazama is inside the docker image)
33 *  *   *   *    docker run -v /home/monitoring/Projects/airfire-monitoring-v3-data-ingest:/monitoring/v3 -v /data/monitoring:/monitoring/v3/data -v /home/monitors:/monitoring/v3/data/monitors -w /monitoring/v3 monitoring-v3-data-ingest /monitoring/v3/airsis_createLatestDataframes_exec.R --inputDir=/monitoring/v3/data/monitors --outputDir=/monitoring/v3/data/RData --logDir=/monitoring/v3/data/logs --spatialDataDir=/home/mazama/data/Spatial

# Hourly update of WRCC real-time monitoring data dump (/home/mazama is inside the docker image)
43 *  *   *   *    docker run -v /home/monitoring/Projects/airfire-monitoring-v3-data-ingest:/monitoring/v3 -v /data/monitoring:/monitoring/v3/data -v /home/monitors:/monitoring/v3/data/monitors -w /monitoring/v3 monitoring-v3-data-ingest /monitoring/v3/wrcc_createLatestDataframes_exec.R --inputDir=/monitoring/v3/data/monitors/GOES_push --outputDir=/monitoring/v3/data/RData --logDir=/monitoring/v3/data/logs --spatialDataDir=/home/mazama/data/Spatial
```
