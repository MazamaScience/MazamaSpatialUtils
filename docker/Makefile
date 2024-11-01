################################################################################
# Makefile for building and running docker containers
#
# Version info: first pass . v8 datasets . added US_countryConversion functions
VERSION=0.8.7

production_build:
	docker build --no-cache -t mazamascience/spatialutils:$(VERSION) -t mazamascience/spatialutils:latest .

production_run:
	docker run -ti --rm mazamascience/spatialutils /bin/bash

production_publish:
	docker login && docker push mazamascience/spatialutils:$(VERSION)

