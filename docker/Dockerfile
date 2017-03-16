FROM rocker/r-ver:3.3.1

#######################################################################
# For MazamaSpatialUtils
RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    git \
    libcurl4-openssl-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libssl-dev \
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
    rgeos \
  && installGithub.r \
    mazamascience/MazamaSpatialUtils

# Add spatial data required by MazamaSpatialUtils
RUN mkdir -p /home/mazama/data \
  && wget -nv http://mazamascience.com/RData/Spatial.tar.gz \
  && tar -xzf Spatial.tar.gz -C /home/mazama/data \
  && rm Spatial.tar.gz

