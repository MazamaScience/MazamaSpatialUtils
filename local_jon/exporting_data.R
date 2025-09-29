# Exporting version 0.8 data

library(MazamaCoreUtils)
library(MazamaSpatialUtils)
library(sf)

source('global_vars.R')

setSpatialDataDir("~/Data/Spatial")

datasetNames <-
  installedSpatialData(verbose = FALSE) %>%
  stringr::str_subset("^Simple", negate = TRUE) %>%
  stringr::str_subset("WBD", negate = TRUE) %>%
  stringr::str_subset("_01$", negate = TRUE)

# Create a DB connection
db_conn <- DBI::dbConnect(
  RPostgres::Postgres(),
  host = PGHOST,
  port = PGPORT,
  dbname = PGDATABASE,
  user = PGUSER,
  password = PGPASSWORD,
  sslmode = "require"
)

for (datasetName in datasetNames) {

  message(sprintf("Working on %s ...", datasetName))

  dataset_name <- datasetName %>% snakecase::to_snake_case()

  SPDF <-
    get(loadSpatialData(paste0(datasetName, ".rda"))) %>%
    # Reproject to World Geodetic System: https://epsg.io/4326
    sf::st_transform(sf::st_crs(4326))

  # Convert column names to snake_case
  column_names <- colnames(SPDF) %>% snakecase::to_snake_case()
  colnames(SPDF) <- column_names

  # Drop any duplicates we may have created
  keepers <- !duplicated(column_names)
  SPDF <- SPDF[,keepers]
  # Replace any existing 'id' with 'polygon_id', then remove 'polygon_id'
  SPDF$id <- SPDF$polygon_id
  SPDF$polygon_id <- NULL

  sf::st_write(
    obj = SPDF,
    dsn = db_conn,
    layer = RPostgres::Id(table = dataset_name, schema = "geographic_data"),
    append = FALSE,
    delete_layer = TRUE,
    geometry_name = "geometry"
  )

}



