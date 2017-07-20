
# Load data
simpleCountries <- get("SimpleCountries")
loadSpatialData("EEZCountries")

library(rbenchmark)

# assign some points in known locations
longitude <- c(-87.068929, 73.580593, 26.222891, -134.957874, 178.767389, -75.543444, -171.846830, 147.893274, 120.915880, -123.036491)
latitude <- c(5.523257, -52.916610, 35.855864, -23.243196, -49.687464, -48.293240, -9.203242, -40.137571, 38.394420, 48.732466)
countryCode <- c('CR', 'HM', 'GR', 'PF', 'NZ', 'CL', 'TK', 'AU', 'CN', 'US')

# Check timing
benchmark(getCountryCode(lon = longitude, lat = latitude, dataset = 'simpleCountries'),
          getCountryCode(lon = longitude, lat = latitude, dataset = 'EEZCountries'), 
          getCountryCode(lon = longitude, lat = latitude, dataset = 'simpleCountries', useBuffering = TRUE),
          replications = 10)

# Check accuracy
getCountryCode(lon = longitude, lat = latitude, dataset = 'simpleCountries')
getCountryCode(lon = longitude, lat = latitude, dataset = 'EEZCountries')
getCountryCode(lon = longitude, lat = latitude, dataset = 'simpleCountries', useBuffering = TRUE)
countryCode