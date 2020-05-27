# This is copied and cleaned up from my RStudio session history. In it you can
# see me interrogating the dataset and trying out different things.
#
# You should load this file into RStudio and run it a line at a time to get
# a sense of what I look for in a dataset.

library(MazamaSpatialUtilts)

setSpatialDataDir("~/Data/Spatial")

# ----- Does the new convert function work? ------------------------------------

# Should remove existing USCensusCounties first.

convertUSCensusCounties()

# ----- Load the newly created data --------------------------------------------

# Clean out the environment
rm(list = ls())

loadSpatialData("USCensusCounties")

# Nice! We see all four datasets in the Environment window.

# ----- Compare simplifications ------------------------------------------------

?plot
?sp::`plot,SpatialPolygons,missing-method`

# Let's try overplotting
subset(USCensusCounties_01, stateCode == "WA") %>% plot(border = "gray90", lwd = 2)
subset(USCensusCounties_02, stateCode == "WA") %>% plot(border = "gray80", lwd = 1.5, add = TRUE)
subset(USCensusCounties_05, stateCode == "WA") %>% plot(border = "gray50", lwd = 1.1, add = TRUE)
subset(USCensusCounties, stateCode == "WA") %>% plot(border = "firebrick", lwd = 0.8, add = TRUE)

# How much difference between 01 and 05/02?
subset(USCensusCounties_01, stateCode == "WA") %>% plot(border = "gray90", lwd = 2)
subset(USCensusCounties_05, stateCode == "WA") %>% plot(border = "gray50", lwd = 1.1, add = TRUE)
subset(USCensusCounties_01, stateCode == "WA") %>% plot(border = "gray90", lwd = 2)
subset(USCensusCounties_02, stateCode == "WA") %>% plot(border = "gray80", lwd = 1.5, add = TRUE)

# What about another state with islands -- Massachussets?
subset(USCensusCounties_02, stateCode == "MA") %>% plot(border = "gray80", lwd = 1.5)
subset(USCensusCounties_05, stateCode == "MA") %>% plot(border = "gray80", lwd = 1.5, add = TRUE)
subset(USCensusCounties, stateCode == "MA") %>% plot(border = "gray80", lwd = 1.5, add = TRUE)

# Is it easier to see with "firebrick?
subset(USCensusCounties_05, stateCode == "MA") %>% plot(border = "gray80", lwd = 1.5)
subset(USCensusCounties, stateCode == "MA") %>% plot(border = "gray80", lwd = 1.5, add = TRUE)
subset(USCensusCounties, stateCode == "MA") %>% plot(border = "firebrick")

# Higher resolutio *under* lower resolution so the deviations show?
subset(USCensusCounties_05, stateCode == "MA") %>% plot(border = "gray80", lwd = 1.2, add = TRUE)

# Were on to something!

subset(USCensusCounties, stateCode == "MA") %>% plot(border = "red", lwd = 0.8)
subset(USCensusCounties_05, stateCode == "MA") %>% plot(border = "gray80", lwd = 1.2, add = TRUE)
subset(USCensusCounties_05, stateCode == "MA") %>% plot(border = "gray90", lwd = 1.2, add = TRUE)
subset(USCensusCounties_05, stateCode == "MA") %>% plot(border = "gray60", lwd = 1.2, add = TRUE)

# Nice! Looks like the 05 dataset is the only reasonable one for counties.
subset(USCensusCounties, stateCode == "MA") %>% plot(border = "red", lwd = 0.8)
subset(USCensusCounties_05, stateCode == "MA") %>% plot(border = "gray60", lwd = 1.2, add = TRUE)

# Let's try other states with islands

subset(USCensusCounties, stateCode == "VA") %>% plot(border = "red", lwd = 0.8)
subset(USCensusCounties_05, stateCode == "VA") %>% plot(border = "gray60", lwd = 1.2, add = TRUE)

subset(USCensusCounties, stateCode == "DE") %>% plot(border = "red", lwd = 0.8)
subset(USCensusCounties_05, stateCode == "DE") %>% plot(border = "gray60", lwd = 1.2, add = TRUE)

subset(USCensusCounties, stateCode == "TX") %>% plot(border = "red", lwd = 0.8)
subset(USCensusCounties_05, stateCode == "TX") %>% plot(border = "gray60", lwd = 1.2, add = TRUE)

# A state without islands?
subset(USCensusCounties, stateCode == "KS") %>% plot(border = "red", lwd = 0.8)
subset(USCensusCounties_05, stateCode == "KS") %>% plot(border = "gray60", lwd = 1.2, add = TRUE)
subset(USCensusCounties_02, stateCode == "KS") %>% plot(border = "dodgerblue", lwd = 1.2, add = TRUE)
subset(USCensusCounties_01, stateCode == "KS") %>% plot(border = "dodgerblue", lwd = 1.2, add = TRUE)

# ----- Compare Sizes ----------------------------------------------------------

# The 01 and 02 datasets aren't much smaller than the 05.
# Let's dig a little deeper into the sizes of components.

slotNames(USCensusCounties_01)
pryr::object_size(USCensusCounties_01@data)
pryr::object_size(USCensusCounties_01@polygons)
pryr::object_size(USCensusCounties_02@polygons)
pryr::object_size(USCensusCounties_05@polygons)
pryr::object_size(USCensusCounties@polygons)
pryr::object_size(USCensusCounties@proj4string)
pryr::object_size(USCensusCounties@bbox)
pryr::object_size(USCensusCounties@plotOrder)
pryr::object_size(USCensusCounties@polygons)
pryr::object_size(USCensusCounties@data)
pryr::object_size(USCensusCounties)
pryr::object_size(USCensusCounties_05)
pryr::object_size(USCensusCounties_02)
pryr::object_size(USCensusCounties_01)

# ----- Review @data slot ------------------------------------------------------

# Did we properly convert spanish language names?
library(dplyr)

# Check out Puerto Rico county names
USCensusCounties@data %>% filter(stateCode == "PR") %>% pull(countyName)

# Do we have expected stateCodes?
USCensusCounties@data %>% pull(stateCode) %>% sort() %>% unique()

# Are all stateCodes in the package internal 'US_52' set of codes?

# NOTE:  I use 'bop' as a throw-away temporary variable
bop <- USCensusCounties@data %>% pull(stateCode) %>% sort() %>% unique()
all(bop %in% US_52)
which(!bop %in% US_52)
bop[which(!bop %in% US_52)]

# Use the new US_stateCodeToName() function
US_stateCodeToName( bop[which(!bop %in% US_52)] )

# Nice.

# Do all columns in this SPDF have data?

# NOTE:  For this we create a function to be applied to every column in the 
# NOTE:  dataframe. Note that dataframe can be interpreted as a List of vectors.
f <- function(x) { any(is.na(x)) }
lapply(USCensusCounties@data, f) %>% str()

# No missing values. Excellent!

# Lets plot CONUS just for fun. (Knowning that the full resolution plot might 
# take a while.)

USCensusCounties_01 %>% subset(stateCode %in% CONUS) %>% plot()

# Let's make it prettier using the tmap package 
# You will be learning how to do this very soon.

# Use 'grep' to find where I had done this before.

# Code copied fand modified rom the COVID-19 repository: COVID-19/maps/JHU_deaths.R

library(tmap)

conus_proj <- sp::CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

counties_01 <- subset(USCensusCounties_01, stateCode %in% CONUS)
counties_02 <- subset(USCensusCounties_02, stateCode %in% CONUS)
counties_05 <- subset(USCensusCounties_05, stateCode %in% CONUS)
counties <- subset(USCensusCounties, stateCode %in% CONUS)

tm_shape(counties_01, projection = conus_proj) +
  tm_polygons()

# Fine, but I don't want the color fill

# ?tm_polygons

# Try this
tm_shape(counties_01, projection = conus_proj) +
  tm_borders()

# A warning message but still got a nice plot. Add a title.

tm_shape(counties_01, projection = conus_proj) +
  tm_borders() +
  tm_layout(
    title = "USCensusCounties_01",
    title.size = 1.1,
    title.position = c("center", "top"),
    frame = FALSE
  )

# Let's risk waiting a while and to it for 05
tm_shape(counties_05, projection = conus_proj) +
  tm_borders() +
  tm_layout(
    title = "USCensusCounties_05",
    title.size = 1.1,
    title.position = c("center", "top"),
    frame = FALSE
  )

# Ouch! Got an error:

# Error in createPolygonsComment(p) : 
#   rgeos_PolyCreateComment: orphaned hole, cannot find containing polygon for hole at index 2

# Not a blocker but perhaps something to add an issue for.

# OK, enough for this morning.


