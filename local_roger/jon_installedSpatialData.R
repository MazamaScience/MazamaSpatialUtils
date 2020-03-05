# installedSpatialData.R

library(dplyr)

#data_dir <- getSpatialDataDir()
data_dir <- getwd()

# Vector of available datasets
datasetNames <- list.files(getSpatialDataDir(), pattern = "*\\.[rR][dD]a?t?a") %>%
  stringr::str_extract(".*[^\\.RData]") %>%  # extract all characters up to ".RData"
  stringr::str_replace("_0\\d$","") %>%      # remove _0# (could use this technique above)
  sort() %>%
  unique()

# List of lists with text info about datasets
datasetInfoList <- list(
  USCensusCBSA = list(
    summary = "U.S. Census Core Based Statistical Areas",
    details = "\
  The USCensusUrbanAreas layer is a SpatialPolygonsDataFrame which represent the \
  urban areas delineated by the US Census that represent densely developed \
  territory."
  ),
  USFSRangerDistricts = list(
    summary = "U.S. Forest Service Ranger districts ",
    details = "\
  The USFSRangerDistricts layer is a SpatialPolygonsDataFrame representing U.S. \
  Forest Service ranger district administrative boundaries. Ranger districts are \
  sub units of National Forests intended to identify the specific organizational \
  units that administer areas."
  )
)

## To avoid concatenating inside a loop (dangerously slow), create a list of
## text strings to be pasted together at the end.
#datasetTextList <- list()
#for ( datasetName in datasetNames ) {
#
#  datasetTextList[[datasetName]] <-
#    message(sprintf(
#      "%s -- %s\n\t%s",
#      datasetName,
#      datasetInfoList[[datasetName]]$summary,
#      datasetInfoList[[datasetName]]$details
#    ))
#
#}

maxLength <- max(stringr::str_count(datasetNames))
formatString <- paste0("%", maxLength, "s -- %s")
datasetTextList <- list()
for ( datasetName in datasetNames ) {
  datasetTextList[[datasetName]] <-
    sprintf(
      formatString,
      datasetName,
      datasetInfoList[[datasetName]]$summary
    )
}
cat(paste(datasetTextList, collapse = "\n"))

