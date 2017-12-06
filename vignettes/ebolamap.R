## ----ebolaCountries, fig.width=7, fig.height=7---------------------------
library(MazamaSpatialUtils)
library(sp)
setSpatialDataDir('~/Data/Spatial') # Use your own Spatial Data directory
loadSpatialData('TMWorldBorders')
df <- TMWorldBorders@data
ebolaCountries <- subset(df,countryName %in% c('Liberia', 'Sierra Leone', 'Guinea'))
countryCodes = ebolaCountries[,2]
ebolaCountries <- subset(TMWorldBorders, countryName %in% c('Liberia', 'Sierra Leone', 'Guinea'))
plot(ebolaCountries, lwd=3)

## ------------------------------------------------------------------------
convertGADM(countryCode=countryCodes[1], admLevel=2)
loadSpatialData('GADM_GN_2')
convertGADM(countryCode=countryCodes[2], admLevel=1)
loadSpatialData('GADM_LR_1')
convertGADM(countryCode=countryCodes[3], admLevel=2)
loadSpatialData('GADM_SL_2')

## ----ebolaTable----------------------------------------------------------
url <- "http://www.johnstonsarchive.net/policy/westafrica-ebola.html"
raw <- xml2::read_html(url)
tables <- rvest::html_nodes(raw, "table")
ebolaTable <- rvest::html_table(tables[[3]])

## ----colors--------------------------------------------------------------
breaks <- c(.003,.01,.03,.1,.3,1,3,10)
colorIndices <- .bincode(ebolaTable[c(2:16,20:34,38:78),4], breaks)
colorIndices[is.na(colorIndices)] <- 9
colors <- c('#8ed150','#a2d840','#c2e529','#fdfd00','#ffd600','#ff8000','#f40000','#97547e','#ffffff')

## ----ebolaCaseRate, fig.width=7, fig.height=10, fig.align='center'-------
plot(GADM_GN_2, col=colors[colorIndices[31:71]])
plot(GADM_LR_1, col=colors[colorIndices[1:15]], add=TRUE)
plot(GADM_SL_2, col=colors[colorIndices[16:30]], add=TRUE)
plot(ebolaCountries, lwd=3, add=TRUE)
legend('bottomleft', legend=rev(breaks[1:7]), fill=rev(colors[1:8]), bty='n', title='case rate per\n1000 pop')
text(-10.8,5.8,'Liberia', font=2)
text(-14.1,7.8,'Sierra Leone', font=2)
text(-14.8,9.8,'Guinea', font=2)
text(-12.5,4.8, 'Rate of Ebola cases by region,\nWest Africa (to 22 Mar 2015)', font=2)
text(-12.5, 4.3,'(both confirmed and unconfirmed cases)', cex=.8, xpd=NA)

