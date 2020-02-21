library(dplyr)


dataDir <- "~/Downloads"

# TODO:  Should have used
#   https://geonames.usgs.gov/docs/stategaz/NationalFile.zip

url <- "https://geonames.usgs.gov/docs/stategaz/AllStates.zip"

filePath <- file.path(dataDir, basename(url))
utils::download.file(url, filePath)

utils::unzip(filePath, exdir = dataDir)

# Individual files have names like "AK_Features_20200101.txt"

stateCode <- "WA"
fileName <- paste0(stateCode, "_Features_20200101.txt")
filePath <- file.path(dataDir, fileName)


# > df <- readr::read_delim(filePath, delim = "|")
# > str(lapply(df, class))
# List of 20
# $ FEATURE_ID     : chr "numeric"      c
# $ FEATURE_NAME   : chr "character"    c
# $ FEATURE_CLASS  : chr "character"    f
# $ STATE_ALPHA    : chr "character"    f
# $ STATE_NUMERIC  : chr "character"    -
# $ COUNTY_NAME    : chr "character"    c
# $ COUNTY_NUMERIC : chr "character"    -
# $ PRIMARY_LAT_DMS: chr "character"    -
# $ PRIM_LONG_DMS  : chr "character"    -
# $ PRIM_LAT_DEC   : chr "numeric"      n
# $ PRIM_LONG_DEC  : chr "numeric"      n
# $ SOURCE_LAT_DMS : chr "character"    -
# $ SOURCE_LONG_DMS: chr "character"    -
# $ SOURCE_LAT_DEC : chr "numeric"      -
# $ SOURCE_LONG_DEC: chr "numeric"      -
# $ ELEV_IN_M      : chr "numeric"      n
# $ ELEV_IN_FT     : chr "numeric"      -
# $ MAP_NAME       : chr "character"    c
# $ DATE_CREATED   : chr "character"    -
# $ DATE_EDITED    : chr "character"    -

col_types <- "ccff-c---nn----n-c--"
col_names <- c(
  "ID",
  "name",
  "class",
  "stateCode",
  "countyName",
  "longitude",
  "latitude",
  "elevation",
  "mapLabel"
)

df <- readr::read_delim(
  filePath, 
  delim = "|", 
  skip = 1,
  col_names = col_names,
  col_types = col_types
)

# > pryr::object_size(df)
# 7.95 MB


# And now some fun:

df %>% 
  filter(class == "Glacier") %>% 
  pull(countyName) %>% 
  table() %>% 
  sort() %>% 
  pie()

df %>% 
  filter(class == "Hospital") %>% 
  pull(countyName) %>% 
  table() %>% 
  sort() %>% 
  pie()

df %>% 
  filter(class == "Military") %>% 
  pull(countyName) %>% 
  table() %>% 
  sort() %>% 
  pie()



