# Create geojson for HUC10 watersheds in the Methow Valley

# TODO:  How did I generate Methow_HUC10.rda

library(sf)

load("./Methow_HUC10.rda")

plot(Methow$geometry)
text(Methow$longitude, Methow$latitude, Methow$HUCName)

Methow_02 <-
  Methow %>%
  MazamaSpatialUtils::simplify(keep = 0.02)

geojsonio::geojson_write(Methow_02, file = "Methow_HUC10_02.geojson")



