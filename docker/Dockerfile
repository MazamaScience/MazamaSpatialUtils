FROM rocker/r-ver:3.4

#######################################################################
# For MazamaSpatialUtils
RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    git \
    libcurl4-openssl-dev \
    libgdal-dev \
    libgeos-dev \
    libjq-dev \
    libproj-dev \
    libprotobuf-dev \
    libssl-dev \
    libv8-dev \
    protobuf-compiler \
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
    remotes \
    rgdal \
    rgeos \
    rmapshaper \
    MazamaSpatialUtils

# Add spatial data required by MazamaSpatialUtils
RUN mkdir -p /home/mazama/data/Spatial \
  && wget -nv http://mazamascience.com/RData/Spatial/mazama_spatial_files-0.5.tar.gz \
  && tar -xzf mazama_spatial_files-0.5.tar.gz -C /home/mazama/data/Spatial \
  && rm mazama_spatial_files-0.5.tar.gz
