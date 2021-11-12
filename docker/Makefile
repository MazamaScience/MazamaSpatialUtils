################################################################################
# Makefile for building and running docker containers


# PRODUCTION version -----------------------------------------------------------
production_build:
	docker build --no-cache -t mazamascience/spatialutils:0.7.6 -t mazamascience/spatialutils:latest .

production_publish:
	docker login && docker push mazamascience/spatialutils:0.7.6

