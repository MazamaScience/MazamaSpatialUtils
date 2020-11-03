library(MazamaSpatialUtils)
loadSpatialData('OSMTimezones')

# create a random sampling of 500 points, where longitude is between 
# -8 and 35, and latitude is between 39 and 58 (points in or near Europe)
longitude <- runif(500, -8, 35)
latitude  <- runif(500, 39, 58)

# use getTimezone to determine which timezone each point is in using both the old and new timezone shapefiles
OSMTimezones_05 <- rmapshaper::ms_simplify(OSMTimezones, .05)
OSMTimezones_02 <- rmapshaper::ms_simplify(OSMTimezones, .02)
a <- getTimezone(longitude, latitude, dataset = "SimpleTimezones")
b <- getTimezone(longitude, latitude, dataset = "OSMTimezones_05")

# Use rbenchmark to test the speed of the function using each different dataset
library(rbenchmark)
benchmark(getTimezone(longitude, latitude, dataset = "SimpleTimezones"), 
          getTimezone(longitude, latitude, dataset = "OSMTimezones_02"),
          getTimezone(longitude, latitude, dataset = "OSMTimezones_05"))

#                                                            test replications elapsed relative user.self sys.self user.child
# 2 getTimezone(longitude, latitude, dataset = "OSMTimezones_02")          100  12.534    1.000    12.291    0.232      0.000
# 3 getTimezone(longitude, latitude, dataset = "OSMTimezones_05")          100  13.892    1.108    13.246    0.621      0.004
# 1 getTimezone(longitude, latitude, dataset = "SimpleTimezones")          100  16.196    1.292    15.980    0.187      0.000

# OSMTimezones_02 is slightly faster. 
# What if we bump up the number of points to identify to 10,000?

longitude <- runif(10000, -8, 35)
latitude  <- runif(10000, 39, 58)

benchmark(getTimezone(longitude, latitude, dataset = "SimpleTimezones"), 
          getTimezone(longitude, latitude, dataset = "OSMTimezones_02"),
          getTimezone(longitude, latitude, dataset = "OSMTimezones_05"), 
          replications = 10)

#                                                            test replications elapsed relative user.self sys.self user.child
# 2 getTimezone(longitude, latitude, dataset = "OSMTimezones_02")           10  45.055    1.069    28.645   16.175      0.045
# 3 getTimezone(longitude, latitude, dataset = "OSMTimezones_05")           10  44.702    1.061    28.964   15.703      0.000
# 1 getTimezone(longitude, latitude, dataset = "SimpleTimezones")           10  42.136    1.000    27.311   14.657      0.000


# Now SimpleTimezones is faster. OSMTimezones_02 and OSMTimezones_05 are almost exactly the same. 
# Try with points scattered about the globe, not just in Europe

longitude <- runif(500, -180, 180)
latitude <- runif(500, -90, 90)
benchmark(getTimezone(longitude, latitude, dataset = "SimpleTimezones"), 
          getTimezone(longitude, latitude, dataset = "OSMTimezones_02"), 
          getTimezone(longitude, latitude, dataset = "OSMTimezones_05"))


# test replications elapsed relative user.self sys.self user.child
# 2 getTimezone(longitude, latitude, dataset = "OSMTimezones_02")          100  12.814    1.000    12.560    0.242          0
# 3 getTimezone(longitude, latitude, dataset = "OSMTimezones_05")          100  13.923    1.087    13.464    0.414          0
# 1 getTimezone(longitude, latitude, dataset = "SimpleTimezones")          100  16.393    1.279    16.110    0.280          0

# Now OSMTimezones_02 is significantly faster.

