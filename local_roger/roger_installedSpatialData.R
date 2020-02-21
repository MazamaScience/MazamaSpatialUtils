# installedSpatialData()
#
# This returns all of the .RData files in the Data dir and displays them like so.
#
# The following Spatial Datasets are installed:
#   EEZCountries - country boundaries including Exclusive Economic Zones
#   EPARegions - U.S. EPA region boundaries
#   Foo -
#   HIFLDFederalLands - U.S. Federal Lands
#   MTBSBurnAreas - MTBS Burn Areas from 1984 - 2017
#   NaturalEarthAdm1 - state/province/oblast level boundaries
#   OSMTimezones - Open Street Map high resolution time zones
#   TMWorldBorders - high resolution country level boundaries
#   TMWorldBorders_01 -
#   TMWorldBorders_05 -
#   USCensusCounties - US county level boundaries
#   USCensusStates - US state level boundaries
#   USCensusUrbanAreas - U.S. Census Urban Areas
#   USFSRangerDistricts - U.S. Forest Service Ranger districts
#   WorldTimezones - high resolution timezones
#
# Datasets which lack a description are still shown.

library(dplyr)

# Taken from dataset documentation
datsetInventory <- list(
  "SimpleCountries" = "country outlines",
  "SimpleCountriesEEZ" = "country outlines including Exclusive Economic Zones over water",
  "SimpleTimezones" = "timezones",
  "EEZCountries" = "country boundaries including Exclusive Economic Zones",
  "NaturalEarthAdm1" = "state/province/oblast level boundaries",
  "OSMTimezones" = "Open Street Map high resolution time zones",
  "OSMTimezones_05" = "low resolution Open Street Map time zones suitable for small-scale maps",
  "TMWorldBorders" = "high resolution country level boundaries",
  "TerrestrialEcoregions" = "terrestrial eco-regions",
  "TerrestrialEcoregions_05" = "low resolution terrestrial eco-regions suitable for small-scale maps",
  "USCensus115thCongress" = "US congressional districts",
  "USCensusCounties" = "US county level boundaries",
  "USCensusStates" = "US state level boundaries",
  "USIndianLands" = "US tribal boundaries",
  "WorldTimezones" = "high resolution timezones",
  "CA_AirBasins"= "California regional air basin boundaries",
  "EPARegions"= "U.S. EPA region boundaries",
  "GACC" = "Geographic Area Coordination Center (GACC) boundaries",
  "GADM" = "GADM administrative area boundaries",
  "HIFLDFederalLands" = "U.S. Federal Lands",
  "HMSSmoke" = "NOAA Hazard Mapping System Smoke (HMSS) areas",
  "MTBSBurnAreas" = "MTBS Burn Areas from 1984 - 2017",
  "NWSFireZones" = "NWS weather forecast zones",
  "PHDs" = "Public Health Districts for Washington, Oregon, Idaho, and California",
  "SimpleCountries" = "Simplified version of the TMWorldBorders shapefile",
  "SimpleCountriesEEZ" = "Simplified version of EEZCountries",
  "SimpleTimezones" = "Timezones of the world",
  "HouseLegislativeDistricts" = "U.S. State Legislative Districts, by chamber",
  "TMWorldBordersSimple" = "Simplified version of the TMWorldBorders layer",
  "USCensusCBSA" = "U.S. Census Core Based Statistical Areas",
  "USCensusUrbanAreas" = "U.S. Census Urban Areas",
  "USFSRangerDistricts" = "U.S. Forest Service Ranger districts",
  "WBDHU" = "Watershed boundary hydrologic units",
  "weatherZones" = "NWS public weather forecast zones",
  "WorldEEZ" = "World Exclusive Economic Zones Boundaries of countries")

installed_datasets <- list.files(getSpatialDataDir(), pattern = "*\\.[rR][dD]a?t?a") %>%
  stringr::str_extract(".*[^\\.RData]") %>%  # extract all characters up to ".RData"
  #stringr::str_replace("_0\\d$","") %>%      # remove _0# (could use this technique above)
  sort() %>%
  unique()

message("The following Spatial Datasets are installed:")

for (dataset in installed_datasets) {
  message(sprintf("  %s - %s", dataset, suppressWarnings(stringr::str_replace(datsetInventory[dataset], "NULL", ""))))
}
