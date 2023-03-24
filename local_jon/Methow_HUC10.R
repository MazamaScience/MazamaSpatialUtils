# Create geojson for HUC10 watersheds in the Methow

library(MazamaSpatialUtils)
setSpatialDataDir("~/Data/Spatial_0.8")

# ----- Find approprate HUC6 for Winthrop: 48.48, -120.19 -----

longitude <- -120.19
latitude <- 48.48

loadSpatialData("WBDHU6_01")
getHUC(longitude, latitude, "WBDHU6_01")
# 170200
getHUCName(longitude, latitude, "WBDHU6_01")
# Upper Columbia


# ----- Use this to speed up the HUC8 search -----

loadSpatialData("WBDHU8") # no simplified version available
getHUC(longitude, latitude, "WBDHU8", HUCs = "170200")
# 17020008
getHUCName(longitude, latitude, "WBDHU8", HUCs = "170200")
# Methow


# ----- Now generate the simplified HUC10 subset -----

loadSpatialData("WBDHU10") # no simplified version available
Methow <-
  WBDHU10 %>%
  dplyr::filter(stringr::str_detect(HUC, "^17020008")) %>%
  MazamaSpatialUtils::simplify(keep = 0.02)


# ----- Save as .rda and .geojson -----

save(Methow, file = "Methow_HUC10_02.rda")
geojsonio::geojson_write(Methow, file = "Methow_HUC10_02.geojson")


if ( FALSE ) {

  library(sf)
  load("Methow_HUC10_02.rda")
  plot(Methow$geometry)
  text(Methow$longitude, Methow$latitude, labels = Methow$HUCName)
  # points(longitude, latitude, pch = 16, col = 'red')
  # text(longitude, latitude, pos = 1, labels = "Winthrop")

}

