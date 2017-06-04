# nbi_cleanup.R
#
# This script creates a cleaned up dataframe from the raw NBI dataset.
#
# Information on column formats is here:
#
# https://www.fhwa.dot.gov/bridge/mtguide.pdf

library(maps) # for FIPS state codes

###download.file('https://www.fhwa.dot.gov/bridge/nbi/2016hwybronlyonefile.zip','nbi_2016.zip')

# Read everything in as raw character data to avoid any 'problems(...)'
col_types <- paste0(rep('c',135),collapse='')
rawDF <- readr::read_csv('nbi_2016.zip', col_types=col_types)

# Create a minimal nbi 'tibble' that has the proper number of rows
nbi <- rawDF[,'STATE_CODE_001']

# ----- stateCode -------------------------------------------------------------

# From the coding guide:
#
#   Item 1 - State Code     3 digits
#   The first 2 digits are the Federal Information Processing Standards
#   (FIPS) code for States, and the third digit is the FHWA region code.
#   (New Jersey and New York will retain an FHWA region code of 2.)

fips <- stringr::str_sub(rawDF$STATE_CODE_001,1,2)

# Create a vector of state codes, named by fips codes
stateByFips <- as.character(maps::state.fips$abb)
names(stateByFips) <- sprintf("%02d", maps::state.fips$fips)
stateByFips <- c(stateByFips, '72'='PR')

nbi$stateCode <- stateByFips[fips]

# Now remove the unneeded 'STATE_CODE_001'
nbi$STATE_CODE_001 <- NULL

# test
# barplot(sort(table(nbi$stateCode)), horiz=TRUE, las=1)

# ----- countyCode ------------------------------------------------------------

# TODO

# ----- latitude --------------------------------------------------------------

# From the coding guide:
#
#   Item 16 - Latitude  (XX degrees XX minutes XX.XX seconds)     8 digits
#   For bridges on STRAHNET and STRAHNET Connector highways and on the NHS,
#   record and code the latitude of each in degrees, minutes and seconds to
#   the nearest hundredth of a second (with an assumed decimal point).

latDMS <- rawDF$LAT_016

# > table(stringr::str_count(latDMS)
#
# 7      8 
# 1 614379 
#
# > which(stringr::str_count(latDMS) == 7)
# [1] 333453
#
# row 333453 has a latitude that is missing it's last character -- just add '0'
latDMS[333453] <- paste0(latDMS[333453], '0')

# Convert '00000000' to NA
badMask <- stringr::str_detect(latDMS,'00000000')
latDMS[badMask] <- NA

deg <- as.numeric(stringr::str_sub(latDMS, 1, 2))
min <- as.numeric(stringr::str_sub(latDMS, 3, 4))
sec <- as.numeric(stringr::str_sub(latDMS, 5, 8))/100

nbi$latitude <- deg + min/60 + sec/3600

# Remove values that are out of domain

badMask <- nbi$latitude < 10 | nbi$latitude > 80
nbi$latitude[badMask] <- NA

# ----- longitude -------------------------------------------------------------

#   Item 17 - Longitude  (XX degrees XX minutes XX.XX seconds)     9 digits
#   For bridges on STRAHNET and STRAHNET Connector highways and on the NHS,
#   record and code the longitude of each in degrees, minutes and seconds to
#   the nearest hundredth of a second (with an assumed decimal point).  A
#   leading zero shall be coded where needed.

lonDMS <- rawDF$LONG_017

# > sum(is.na(lonDMS))
# [1] 8

# Convert NA to '000000000'
lonDMS[is.na(lonDMS)] <- '000000000'

# > table(stringr::str_count(lonDMS))
# 
# 8      9 
# 16879 597500 
#
# > which(stringr::str_count(lonDMS) == 8)[1:3]
# [1] 25741 25742 25743
#
# Looks like many 8-char values are missing the initial '0'
#
# hist(as.numeric(lonDMSshortMask])) -- yep, mostly 80 and 90 degrees with a couple of zeros we'll remove later

shortMask <- stringr::str_count(lonDMS) == 8
lonDMS[shortMask] <- paste0('0', lonDMS[shortMask])


# NOTE:  these are degrees W

deg <- as.numeric(stringr::str_sub(lonDMS, 1, 3))
min <- as.numeric(stringr::str_sub(lonDMS, 4, 5))
sec <- as.numeric(stringr::str_sub(lonDMS, 6, 9))/100

nbi$longitude <- -1 * (deg + min/60 + sec/3600)

# Remove values that are out of domain

badMask <- nbi$longitude < -180 | nbi$longitude > -10
nbi$longitude[badMask] <- NA

# -----------------------------------------------------------------------------
# quick test
# -----------------------------------------------------------------------------

wa <- dplyr::filter(nbi, stateCode == 'WA')
dim(wa)
plot(wa$longitude, wa$latitude, pch=0)
which(wa$longitude > -100)
wa <- wa[-3531,]
plot(wa$longitude, wa$latitude, pch=0)


# -----------------------------------------------------------------------------
