# https://hub.docker.com/r/rocker/geospatial/tags
FROM rocker/geospatial:4.2
MAINTAINER Jonathan Callahan <jonathan@mazamascience.com>


# For MazamaSpatialUtils
RUN install2.r --error -r "https://cran.rstudio.com" \
    rmapshaper

RUN installGithub.r \
    MazamaSpatialUtils@0.8.3

# Add potentially useful spatial data for MazamaSpatialUtils 0.8.x
RUN mkdir -p /home/mazama/data/Spatial \
  && wget --directory-prefix /home/mazama/data/Spatial --no-host-directories --cut-dirs=2 http://data.mazamascience.com/MazamaSpatialUtils/Spatial_0.8/EEZCountries.rda \
  && wget --directory-prefix /home/mazama/data/Spatial --no-host-directories --cut-dirs=2 http://data.mazamascience.com/MazamaSpatialUtils/Spatial_0.8/EEZCountries_02.rda \
  && wget --directory-prefix /home/mazama/data/Spatial --no-host-directories --cut-dirs=2 http://data.mazamascience.com/MazamaSpatialUtils/Spatial_0.8/EEZCountries_05.rda \
  && wget --directory-prefix /home/mazama/data/Spatial --no-host-directories --cut-dirs=2 http://data.mazamascience.com/MazamaSpatialUtils/Spatial_0.8/EPARegions.rda \
  && wget --directory-prefix /home/mazama/data/Spatial --no-host-directories --cut-dirs=2 http://data.mazamascience.com/MazamaSpatialUtils/Spatial_0.8/EPARegions_02.rda \
  && wget --directory-prefix /home/mazama/data/Spatial --no-host-directories --cut-dirs=2 http://data.mazamascience.com/MazamaSpatialUtils/Spatial_0.8/EPARegions_05.rda \
  && wget --directory-prefix /home/mazama/data/Spatial --no-host-directories --cut-dirs=2 http://data.mazamascience.com/MazamaSpatialUtils/Spatial_0.8/NaturalEarthAdm1.rda \
  && wget --directory-prefix /home/mazama/data/Spatial --no-host-directories --cut-dirs=2 http://data.mazamascience.com/MazamaSpatialUtils/Spatial_0.8/NaturalEarthAdm1_02.rda \
  && wget --directory-prefix /home/mazama/data/Spatial --no-host-directories --cut-dirs=2 http://data.mazamascience.com/MazamaSpatialUtils/Spatial_0.8/NaturalEarthAdm1_05.rda \
  && wget --directory-prefix /home/mazama/data/Spatial --no-host-directories --cut-dirs=2 http://data.mazamascience.com/MazamaSpatialUtils/Spatial_0.8/OSMTimezones.rda \
  && wget --directory-prefix /home/mazama/data/Spatial --no-host-directories --cut-dirs=2 http://data.mazamascience.com/MazamaSpatialUtils/Spatial_0.8/OSMTimezones_02.rda \
  && wget --directory-prefix /home/mazama/data/Spatial --no-host-directories --cut-dirs=2 http://data.mazamascience.com/MazamaSpatialUtils/Spatial_0.8/OSMTimezones_05.rda \
  && wget --directory-prefix /home/mazama/data/Spatial --no-host-directories --cut-dirs=2 http://data.mazamascience.com/MazamaSpatialUtils/Spatial_0.8/TMWorldBorders.rda \
  && wget --directory-prefix /home/mazama/data/Spatial --no-host-directories --cut-dirs=2 http://data.mazamascience.com/MazamaSpatialUtils/Spatial_0.8/TMWorldBorders_02.rda \
  && wget --directory-prefix /home/mazama/data/Spatial --no-host-directories --cut-dirs=2 http://data.mazamascience.com/MazamaSpatialUtils/Spatial_0.8/TMWorldBorders_05.rda \
  && wget --directory-prefix /home/mazama/data/Spatial --no-host-directories --cut-dirs=2 http://data.mazamascience.com/MazamaSpatialUtils/Spatial_0.8/USCensusCBSA.rda \
  && wget --directory-prefix /home/mazama/data/Spatial --no-host-directories --cut-dirs=2 http://data.mazamascience.com/MazamaSpatialUtils/Spatial_0.8/USCensusCBSA_02.rda \
  && wget --directory-prefix /home/mazama/data/Spatial --no-host-directories --cut-dirs=2 http://data.mazamascience.com/MazamaSpatialUtils/Spatial_0.8/USCensusCBSA_05.rda \
  && wget --directory-prefix /home/mazama/data/Spatial --no-host-directories --cut-dirs=2 http://data.mazamascience.com/MazamaSpatialUtils/Spatial_0.8/USCensusCounties.rda \
  && wget --directory-prefix /home/mazama/data/Spatial --no-host-directories --cut-dirs=2 http://data.mazamascience.com/MazamaSpatialUtils/Spatial_0.8/USCensusCounties_02.rda \
  && wget --directory-prefix /home/mazama/data/Spatial --no-host-directories --cut-dirs=2 http://data.mazamascience.com/MazamaSpatialUtils/Spatial_0.8/USCensusCounties_05.rda \
  && wget --directory-prefix /home/mazama/data/Spatial --no-host-directories --cut-dirs=2 http://data.mazamascience.com/MazamaSpatialUtils/Spatial_0.8/USCensusStates.rda \
  && wget --directory-prefix /home/mazama/data/Spatial --no-host-directories --cut-dirs=2 http://data.mazamascience.com/MazamaSpatialUtils/Spatial_0.8/USCensusStates_02.rda \
  && wget --directory-prefix /home/mazama/data/Spatial --no-host-directories --cut-dirs=2 http://data.mazamascience.com/MazamaSpatialUtils/Spatial_0.8/USCensusStates_05.rda \
  && wget --directory-prefix /home/mazama/data/Spatial --no-host-directories --cut-dirs=2 http://data.mazamascience.com/MazamaSpatialUtils/Spatial_0.8/WeatherZones.rda \
  && wget --directory-prefix /home/mazama/data/Spatial --no-host-directories --cut-dirs=2 http://data.mazamascience.com/MazamaSpatialUtils/Spatial_0.8/WeatherZones_02.rda \
  && wget --directory-prefix /home/mazama/data/Spatial --no-host-directories --cut-dirs=2 http://data.mazamascience.com/MazamaSpatialUtils/Spatial_0.8/WeatherZones_05.rda


