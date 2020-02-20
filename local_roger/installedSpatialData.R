# installedSpatialData()
#
# Should invisibly return the full set of all installed data including 
# simplified versions by default.  If verbose = TRUE (default), it can spit out 
# a human readable, nicely formatted bunch of text using the descriptions in 
# harmonizedDatasets.

# Taken from dataset documentation
# TODO: Needs additional simplified versions added to the list
harmonizedDatsets <- list(
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

installed_datasets <- list.files(getSpatialDataDir()) %>% 
  stringr::str_extract(".*[^\\.RData]") %>%  # extract all characters up to ".RData"
  stringr::str_replace("_0\\d$","") %>%      # remove _0# (could use this technique above)
  sort() %>% 
  unique()

# yields:
# > installed_datasets
# [1] "EEZCountries"        "EPARegions.rd"       "HIFLDFederalLands"   "MTBSBurnAre"        
# [5] "NaturalEarthAdm1"    "OSMTimezones"        "TMWorldBorders"      "USCensusCounties"   
# [9] "USCensusStates"      "USCensusUrbanAreas"  "USFSRangerDistricts" "WorldTimezones"    

# NOTE: This output is not quite right, as it doesn't strip the ".rd" on EPARegions and 
# eliminates the _01..._05 versions, which contradicts the instructions for, 
# "all installed data including simplified versions by default"
