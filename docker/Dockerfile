FROM rocker/tidyverse:3.5.2
MAINTAINER Jonathan Callahan <jonathan@mazamascience.com>

################################################################################
# From https://github.com/rocker-org/rocker-versioned/blob/master/verse/3.5.2/Dockerfile

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
   # ...
    ## for V8-based javascript wrappers
    libv8-dev # libv8-3.14-dev
   # ....


################################################################################
# From https://hub.docker.com/r/rocker/geospatial/dockerfile
#
# We will run most of the steps in the rocker/geospatial:3.5.2 image but leave 
# off the parts we don't need that make it so huge.
# 
# We are also avoid unnecessary system libraries by starting with
#   FROM rocker/tidyverse:3.5.2
# instead of
#   FROM rocker/verse:3.5.2

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
   # lbzip2 \
   # libfftw3-dev \
    libgdal-dev \
    libgeos-dev \
   # libgsl0-dev \
   # libgl1-mesa-dev \
   # libglu1-mesa-dev \
   # libhdf4-alt-dev \
   # libhdf5-dev \
    libjq-dev \
   # liblwgeom-dev \
    libproj-dev \
    libprotobuf-dev \
   # libnetcdf-dev \
   # libsqlite3-dev \
    libssl-dev \
    libudunits2-dev \
   # netcdf-bin \
    protobuf-compiler \
   # tk-dev \
   # unixodbc-dev \
  && install2.r --error \
    RColorBrewer \
   # RandomFields \
   # RNetCDF \
   # classInt \
   # deldir \
   # gstat \
   # hdf5r \
   # lidR \
    mapdata \
    maptools \
   # mapview \
   # ncdf4 \
   # proj4 \
   # raster \
    rgdal \
    rgeos 
   # rlas \
   # sf \
   # sp \
   # spacetime \
   # spatstat \
   # spdep \
   # geoR \
   # geosphere \
  ## from bioconductor && R -e "BiocManager::install('rhdf5')"


################################################################################
# For MazamaSpatialUtils

RUN install2.r --error -r "https://cran.rstudio.com" \
    maps \
    rmapshaper \
    MazamaSpatialUtils

# Add spatial data required by MazamaSpatialUtils
RUN mkdir -p /home/mazama/data/Spatial \
  && wget -nv http://mazamascience.com/RData/Spatial/mazama_spatial_files-0.6.tar.gz \
  && tar -xzf mazama_spatial_files-0.6.tar.gz -C /home/mazama/data/Spatial \
  && rm mazama_spatial_files-0.6.tar.gz
