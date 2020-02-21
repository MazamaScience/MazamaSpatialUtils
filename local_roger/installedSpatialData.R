# List names of installed spatial data sets and optionally provides a short
# description of the dataset represented.

installedSpatialData <- function(
  verbose = FALSE
) {
  
  # Taken from dataset documentation
  harmonizedDatasets <- list(
    "SimpleCountries" = "Country outlines",
    "SimpleCountriesEEZ" = "Country outlines including Exclusive Economic Zones over water",
    "SimpleTimezones" = "World timezone boundaries",
    "EEZCountries" = "Country boundaries including Exclusive Economic Zones",
    "NaturalEarthAdm1" = "State/province/oblast level boundaries",
    "OSMTimezones" = "Open Street Map high resolution time zones",
    "OSMTimezones_05" = "Low resolution Open Street Map time zones suitable for small-scale maps",
    "TMWorldBorders" = "High resolution country level boundaries",
    "TerrestrialEcoregions" = "Terrestrial eco-regions",
    "TerrestrialEcoregions_05" = "Low resolution terrestrial eco-regions suitable for small-scale maps",
    "USCensus115thCongress" = "US congressional districts",
    "USCensusCounties" = "US county level boundaries",
    "USCensusStates" = "US state level boundaries",
    "USIndianLands" = "US tribal boundaries",
    "WorldTimezones" = "High resolution timezones",
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
  
  # Modified from Jon's, but this way keeps both .RData and .rda files, or
  # whatever other extensions get returned
  datasetNames <- list.files(getSpatialDataDir(), 
                             pattern = "*[.][rR][dD]a?t?a") %>%
    tools::file_path_sans_ext() %>% 
    sort() %>%
    unique()
  
  # If verbose set to TRUE, give some additional information
  if (verbose) {
    # Jon's formatting, plus removal of "NULLS" from descriptions
    maxLength <- max(stringr::str_count(datasetNames))
    formatString <- paste0("%", maxLength, "s -- %s")
    datasetTextList <- list()
    for ( datasetName in datasetNames ) {
      info <- harmonizedDatasets[datasetName]
      if (info == "NULL") {
        info = ""
      }
      datasetTextList[[datasetName]] <-
        sprintf(formatString, datasetName, info)
    }
    cat(paste(datasetTextList, collapse = "\n"))
  }
  
  return(invisible(datasetNames))
  
}