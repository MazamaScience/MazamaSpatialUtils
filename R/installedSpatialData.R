#' @export
#'
#' @title List locally installed spatial datasets
#'
#' @param verbose Logical specifying whether or not to print dataset descriptions.
#'
#' @description Searches the directory set with \code{\link{setSpatialDataDir}}
#' for locally installed spatial data and returns a list of dataset names that
#' can be used with \code{\link{loadSpatialData}}.
#'
#' If \code{verbose = TRUE}, a brief description is provided for each locally
#' installed dataset.
#'
#' @return Invisibly returns a vector of dataset names().

installedSpatialData <- function(
  verbose = TRUE
) {

  # ----- Validate parameters --------------------------------------------------

  spatialDataDir <- getSpatialDataDir()

  if ( !dir.exists(spatialDataDir) )
    stop(sprintf("spatialDataDir '%s' does not exist.", spatialDataDir))

  # ----- Dataset descriptions -------------------------------------------------

  harmonizedDatasets <- list(
    "CA_AirBasins" = "California regional air basin boundaries",
    "EEZCountries" = "Country boundaries including Exclusive Economic Zones",
    "EPARegions" = "US EPA region boundaries",
    "GACC" = "Geographic Area Coordination Center (GACC) boundaries",
    "GADM" = "GADM administrative area boundaries",
    "HIFLDFederalLands" = "US federal lands",
    "HMSSmoke" = "NOAA Hazard Mapping System Smoke (HMSS) areas",
    "HouseLegislativeDistricts" = "US state legislative districts, by chamber",
    "MTBSBurnAreas" = "MTBS burn areas from 1984 - 2017",
    "NaturalEarthAdm1" = "State/province/oblast level boundaries",
    "NWSFireZones" = "NWS fire weather forecast zones",
    "OSMTimezones" = "OpenStreetMap time zones",
    "PHDs" = "Public Health Districts for Washington, Oregon, Idaho, and California",
    "SimpleCountries" = "Simplified version of the TMWorldBorders",
    "SimpleCountriesEEZ" = "Simplified version of EEZCountries",
    "SimpleTimezones" = "Simplified version of WorldTimezones",
    "TerrestrialEcoregions" = "Terrestrial eco-regions",
    "TMWorldBorders" = "Country level boundaries",
    "USCensus116thCongress" = "US congressional districts",
    "USCensusCBSA" = "US Core Based Statistical Areas",
    "USCensusCounties" = "US county level boundaries",
    "USCensusStates" = "US state level boundaries",
    "USCensusUrbanAreas" = "US urban areas",
    "USFSRangerDistricts" = "US Forest Service ranger districts",
    "USIndianLands" = "US tribal boundaries",
    "WBDHU2" = "Watershed boundary level-2 hydrologic units",
    "WBDHU4" = "Watershed boundary level-4 hydrologic units",
    "WBDHU6" = "Watershed boundary level-6 hydrologic units",
    "WBDHU8" = "Watershed boundary level-8 hydrologic units",
    "WBDHU10" = "Watershed boundary level-10 hydrologic units",
    "WBDHU12" = "Watershed boundary level-12 hydrologic units",
    "weatherZones" = "NWS public weather forecast zones",
    "WorldEEZ" = "Country boundaries including Exclusive Economic Zones over water",
    "WorldTimezones" = "Timezones"
  )

  # ----- Get dataset names ----------------------------------------------------

  # Finds both .RData and .rda files
  datasetNames <-
    list.files(getSpatialDataDir(), pattern = "*[.][rR][dD]a?t?a") %>%
    tools::file_path_sans_ext() %>%
    sort() %>%
    unique()

  # Add package internal datasets at the beginning
  datasetNames <- c(
    "SimpleCountries",
    "SimpleCountriesEEZ",
    "SimpleTimezones",
    datasetNames
  )

  # ----- Print out descriptions -----------------------------------------------

  if ( verbose ) {

    maxLength <- max(stringr::str_count(datasetNames))
    formatString <- paste0("%", maxLength, "s -- %s %s") # name, info, pct
    datasetTextList <- list()

    for ( datasetName in datasetNames ) {

      # NOTE:  We want to use the harmonized dataset names as patterns so that
      # NOTE:  we can, for example, have "GADM" match various user created
      # NOTE:  datasets like "GADM_NL_2".

      index <- stringr::str_which(
        string = datasetName,
        pattern = names(harmonizedDatasets)
      )

      if ( length(index) == 0 ) {
        info = "Dataset not recognized"
      } else if ( length(index) == 1 ) {
        info <- harmonizedDatasets[[index]]
      } else {
        # Decide which one is a better match -- the longest one
        counts <- stringr::str_count(names(harmonizedDatasets)[index])
        maxCountsIndex <- which(counts == max(counts))
        preferredIndex <- index[maxCountsIndex]
        info <- harmonizedDatasets[[preferredIndex]]
      }

      if ( info == "NULL" ) {
        info = ""
      }

      # Add % simplified by matching the "_01", "_02", or "_05" at the end.
      pctString <- ""
      matchParts <- stringr::str_match(datasetName, "(.*)_0([1-9])")
      if ( !is.na(matchParts[1,3]) ) {
        pctString <- sprintf("(simplified to %s%%)", matchParts[1,3])
      }

      datasetTextList[[datasetName]] <-
        sprintf(formatString, datasetName, info, pctString)

    }

    datasetTextString <- paste(datasetTextList, collapse = "\n")

    cat(paste0(datasetTextString, "\n"))

  }

  # ----- Return ---------------------------------------------------------------

  return(invisible(datasetNames))

}
