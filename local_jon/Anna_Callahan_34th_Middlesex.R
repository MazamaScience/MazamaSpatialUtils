# 34'th Middlesex legislative district

library(MazamaSpatialUtils)
setSpatialDataDir("~/Data/Spatial")

# Create MA house SFDF
spdfName <- convertStateLegislativeDistricts("MA", "Lower", simplify = FALSE)
loadSpatialData(spdfName)

# 34th Middlesex
spdf <- subset(MA_LowerHouseLegislativeDistricts, legislativeDistrict == "34th Middlesex")

# Looks good
plot(spdf)

# > bbox(spdf)
# min       max
# x -71.13464 -71.07943
# y  42.38736  42.41815

Anna_lat <- 42.4007906
Anna_lon <- -71.1062108

border <- 0.001

# From: https://cfss.uchicago.edu/notes/raster-maps-with-ggmap/

library(ggplot2)
library(ggmap)

spdf_tidy <- broom::tidy(spdf)

medford_stamen <- get_stamenmap(
  bbox = c(
    left = -71.13464 - border,
    bottom = 42.38736 - border,
    right = -71.07943 + border,
    top = 42.41815 + border
  ),
  zoom = 17
)

gg <-
  ggmap(medford_stamen) +
  geom_polygon(
    data = spdf_tidy,
    aes(x = long, y = lat),
    color = 'white',
    alpha = 0.2,
    size = 1,
    fill = 'transparent'
  ) +
  annotate(
    geom = "point",
    x = Anna_lon,
    y = Anna_lat,
    #color = 'red',
    #size = 4
    shape = 24,
    size = 2,
    fill = "red"
  )

print(gg)

# Manually exported as pdf.
