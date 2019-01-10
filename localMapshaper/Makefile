################################################################################
# Makefile for downloading, converting, and simplifying WBDHUC polygons from 
# https://prd-tnm.s3.amazonaws.com/index.html?prefix=StagedProducts/Hydrography/WBD/National/GDB
#
# SPATIAL_DATA_DIR can be overridden with a command like
# make SPATIAL_DATA_DIR=<your/data/dir> download_wbd
#

# Variables that can be overridden in the command line
SPATIAL_DATA_DIR=~/Data/Spatial

download_wbd: 
	cd $(SPATIAL_DATA_DIR);\
	curl https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/WBD/National/GDB/WBD_National_GDB.zip \
	-O;\
	unzip WBD_National_GDB.zip


convert_to_shapefile:
	cd $(SPATIAL_DATA_DIR);\
	ogr2ogr WBD WBD_National_GDB.gdb -f "ESRI Shapefile"

simplify_hu2:
	cd $(SPATIAL_DATA_DIR);\
	mapshaper WBD/WBDHU2.shp -simplify 2% -o format=shapefile WBDHU2_02.shp;\
    mapshaper WBDHU2_02.shp -simplify 50% -o format=shapefile WBDHU2_01.shp;\
	mv WBDHU2_01.* WBD;mv WBDHU2_02.* WBD;

simplify_hu4:
	cd $(SPATIAL_DATA_DIR);\
	mapshaper WBD/WBDHU4.shp -simplify 2% -o format=shapefile WBDHU4_02.shp;\
    mapshaper WBDHU4_02.shp -simplify 50% -o format=shapefile WBDHU4_01.shp;\
	mv WBDHU4_01.* WBD;mv WBDHU4_02.* WBD;

simplify_hu6:
	cd $(SPATIAL_DATA_DIR);\
	mapshaper WBD/WBDHU6.shp -simplify 2% -o format=shapefile WBDHU6_02.shp;\
    mapshaper WBDHU6_02.shp -simplify 50% -o format=shapefile WBDHU6_01.shp;\
	mv WBDHU6_01.* WBD;mv WBDHU6_02.* WBD;	

simplify_hu8:
	cd $(SPATIAL_DATA_DIR);\
	mapshaper WBD/WBDHU8.shp -simplify 2% -o format=shapefile WBDHU8_02.shp;\
    mapshaper WBDHU8_02.shp -simplify 50% -o format=shapefile WBDHU8_01.shp;\
	mv WBDHU8_01.* WBD;mv WBDHU8_02.* WBD;	

simplify_hu10:
	cd $(SPATIAL_DATA_DIR);\
	mapshaper WBD/WBDHU10.shp -simplify 2% -o format=shapefile WBDHU10_02.shp;\
    mapshaper WBDHU10_02.shp -simplify 50% -o format=shapefile WBDHU10_01.shp;\
	mv WBDHU10_01.* WBD;mv WBDHU10_02.* WBD;	

simplify_hu12:
	cd $(SPATIAL_DATA_DIR);\
	mapshaper WBD/WBDHU12.shp -simplify 2% -o format=shapefile WBDHU12_02.shp;\
    mapshaper WBDHU12_02.shp -simplify 50% -o format=shapefile WBDHU12_01.shp;\
	mv WBDHU12_01.* WBD;mv WBDHU12_02.* WBD;

simplify_hu14:	
	cd $(SPATIAL_DATA_DIR);\
	mapshaper WBD/WBDHU14.shp -simplify 2% -o format=shapefile WBDHU14_02.shp;\
    mapshaper WBDHU14_02.shp -simplify 50% -o format=shapefile WBDHU14_01.shp;\
	mv WBDHU14_01.* WBD;mv WBDHU14_02.* WBD;

simplify_all: simplify_hu2 simplify_hu4 simplify_hu6 simplify_hu8 simplify_hu10 simplify_hu12 simplify_hu14
