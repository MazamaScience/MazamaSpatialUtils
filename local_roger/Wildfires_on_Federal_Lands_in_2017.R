# Federal Lands + MTBS Burn Areas Example
# =======================================

library(MazamaSpatialUtils)
library(rmapshaper)
library(raster)

setSpatialDataDir("~/Data/Spatial")
loadSpatialData("USCensusStates")
loadSpatialData("HIFLDFederalLands")
loadSpatialData("MTBSBurnArea")

# ---- Define western states ---------------------------------------------------
west_codes <- c("WA", "ID", "MT", "WY", "CO", "UT", "NM", "AZ", "NV", "CA", "OR")

# ---- # create a subset of Western states ------------------ ------------------
west_states <- subset(USCensusStates, (USCensusStates@data$stateCode %in% 
                                         west_codes))

# these state borders are too detailed. Simplifying to 1%
west_states_01 <- rmapshaper::ms_simplify(west_states, 0.01)
west_states_01@data$rmapshaperid <- NULL

# ---- Get NPS and FS lands out of the HIFLDFederalLands data ------------------
fed_lands <- subset(HIFLDFederalLands, HIFLDFederalLands@data$agencyCode %in% 
                      c("NPS", "FS"))
# Select only the fed lands in western states
fed_lands <- subset(fed_lands, (fed_lands@data$stateCode %in% west_codes))

#reset the projections for all of our spatial data
merc_west_states <- spTransform(west_states_01, CRS("+init=epsg:3857"))
merc_fed_lands <- spTransform(fed_lands, CRS("+init=epsg:3857"))

# Color fed_lands by agency
dev.off()
agency_idx <- as.factor(merc_fed_lands@data$agencyCode)
agency_idx <- factor(agency_idx, levels = c("FS", "NPS"))
color_list <- c("forestgreen", "goldenrod")
plot(merc_west_states, col = "grey90", border = "white")
plot(merc_fed_lands, col = color_list[agency_idx], border = "transparent", add = TRUE)
plot(merc_west_states, col = "transparent", border = "white", add = TRUE)
# Add a legend that matched color to agency
legend("bottomleft", fill = color_list, legend = as.character(unique(agency_idx)))

# ---- Get 2017 burn areas -----------------------------------------------------
loadSpatialData("MTBSBurnArea")
west_fires_2017 <- subset(MTBSBurnArea, MTBSBurnArea@data$year == "2017" 
                          & MTBSBurnArea@data$stateCode %in% west_codes)


# Reproject fires to match the other data dets and plot on top of map
merc_west_fires_2017 <- spTransform(west_fires_2017, CRS("+init=epsg:3857"))
plot(merc_west_fires_2017, col="red", border="transparent", add=TRUE)

# Add a title
title("2017 Fires on NPS and FS Lands", cex = 1.8)
