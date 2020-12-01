# 34'th Middlesex legislative district

library(MazamaSpatialUtils)
setSpatialDataDir("~/Data/Spatial")

# Create MA house SPDF
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


# From: https://cfss.uchicago.edu/notes/raster-maps-with-ggmap/

library(ggmap)

medford_stamen <- get_stamenmap(
  bbox = c(
    left = -71.13464,
    bottom = 42.38736,
    right = -71.07943,
    top = 42.41815
  ),
  zoom = 17
)

ggmap(medford_stamen)

# Manually exported as pdf.
