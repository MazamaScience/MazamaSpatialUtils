
censusTracts <- get(load("local_roger/ca_census_tracts.RData"))

# > head(censusTracts@data)
# STATEFP COUNTYFP TRACTCE             AFFGEOID       GEOID    NAME LSAD     ALAND AWATER
# 0      06      009  000300 1400000US06009000300 06009000300       3   CT 457009794 394122
# 1      06      011  000300 1400000US06011000300 06011000300       3   CT 952744514 195376
# 2      06      013  303102 1400000US06013303102 06013303102 3031.02   CT   6507019      0
# 3      06      013  303202 1400000US06013303202 06013303202 3032.02   CT   3725528      0
# 4      06      013  303203 1400000US06013303203 06013303203 3032.03   CT   6354210      0
# 5      06      013  307102 1400000US06013307102 06013307102 3071.02   CT   1603153      0

# Looks like TRACTCE can be used to aggregate

censusTract@data$ct6 <- stringr::str_sub(censusTract@data$TRACTCE, 1, 6) #default
censusTract@data$ct5 <- stringr::str_sub(censusTract@data$TRACTCE, 1, 5)
censusTract@data$ct4 <- stringr::str_sub(censusTract@data$TRACTCE, 1, 4)
censusTract@data$ct3 <- stringr::str_sub(censusTract@data$TRACTCE, 1, 3)
censusTract@data$ct2 <- stringr::str_sub(censusTract@data$TRACTCE, 1, 2)
censusTract@data$ct1 <- stringr::str_sub(censusTract@data$TRACTCE, 1, 1)

# Now we can use MazamaSpatialUtils::dissolve()
# NOTE:  DOCUMENTATION FOR "field" parameter is oncorrect

ct1 <- MazamaSpatialUtils::dissolve(censusTract, field = "ct1")
plot(ct1)

ct2 <- MazamaSpatialUtils::dissolve(censusTract, field = "ct2")
plot(ct2)

# Etc.
# NOTE:  I had a problem at level 4

# NOTE:  Oops! original columns are dropped when dissolving

# > head(ct2@data)
# ct2
# 1  00
# 2  30
# 3  32
# 4  33
# 5  12
# 6  13
