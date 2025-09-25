# Exporting version 0.8 data

library(MazamaSpatialUtils)

setSpatialDataDir("~/Data/Spatial")
loadSpatialData("EEZCountries_02")

source('global_vars.R')

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

# instance > database > schema
# monitoring > monitoring > geographic_data

sf::st_write(
  obj = EEZCountries_02,
  dsn = db_conn,
  layer = RPostgres::Id(table = "eez_countries_02", schema = "geographic_data"),
  append = FALSE,
  delete_layer = FALSE,
  geometry_name = "geometry"
)


