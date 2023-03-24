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

  # Mazama: 48.592551027443065, -120.4177752781122
  # Winthrop: 48.48015332792662, -120.19323516010063
  # Twisp: 48.36465095793504, -120.11910469051787
  # Methow: 48.12855615502845, -120.01059487138704

  lons <- c(-120.418, -120.193, -120.119, -120.011)
  lats <- c(48.593, 48.480, 48.365, 48.129)
  labels <- c("Mazama", "Winthrop", "Twisp", "Methow")

  points(lons, lats, pch = 16, col = 'red')
  text(lons, lats, labels = labels, pos = 4, col = 'red')



}

