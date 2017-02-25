FROM rocker/r-ver:3.3.2

RUN apt-get update \
  ## Install system libraries. Many more are installed due to dependencies
  && apt-get install -y --no-install-recommends \
    git \
    libcairo2-dev \
    libgdal-dev \
    libgeos-dev \
    libicu-dev \
    libproj-dev \
    libxcb1-dev \
    libxdmcp-dev \
    netcdf-bin \
    wget \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/ \
  && rm -rf /tmp/downloaded_packages/ /tmp/*.rds \
  ## Install R packages from CRAN
  && install2.r --error -r "https://cran.rstudio.com" \
     devtools \
     maps \
     mapdata \
     maptools \
     rgdal \
     rgeos

## Install MazamaSpatialUtils from github
RUN installGithub.r \
     mazamascience/MazamaSpatialUtils

# Add spatial data required by MazamaSpatialUtils
RUN mkdir -p /home/mazama/data \
    && wget -nv http://mazamascience.com/RData/Spatial.tar.gz \
    && tar -xzf Spatial.tar.gz -C /home/mazama/data \
    && rm Spatial.tar.gz

