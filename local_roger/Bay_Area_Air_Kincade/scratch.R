library(sqldf)
# https://stackoverflow.com/questions/47722043/error-rsqlite-fetch-in-r

tracts <- data.frame(TRACTCE=c("x010401", "y010401", "z010401"), stringsAsFactors = FALSE)
# TRACTCE
# 1 x010401
# 2 y010401

pm25 <- read.table(header = TRUE, 
stringsAsFactors = FALSE,
text="
TRACTCE     date    pm25_1day
x010401 20191024      3.73
x010401 20191025      4.71
x010401 20191026      7.04
x010401 20191027      6.50
x010401 20191028      4.11
x010401 20191029      9.46
y010401 20191024      10.0
y010401 20191025      8.0"
)

wide_pm25 <- reshape(pm25, direction = "wide", idvar = "TRACTCE", timevar = "date")

new_tracts <- dplyr::left_join(sf_tracts@data, wide_pm25, by = "TRACTCE")

head(new_tracts)

head(pm25)

subset(pm25, select = c("TRACTCE", "date"))
