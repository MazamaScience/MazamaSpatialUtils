# Plots for blog post on 2020-12-02

library(MazamaSpatialUtils)
setSpatialDataDir("~/Data/Spatial")

loadSpatialData("WorldTimezones")

df <- WorldTimezones@data
df$size <- unlist( lapply(WorldTimezones@polygons, pryr::object_size) )

# View(df)

# Largest is "Europe/Berlin"

Berlin <- subset(WorldTimezones, timezone == "Europe/Berlin")
Berlin_05 <- subset(WorldTimezones_05, timezone == "Europe/Berlin")
Berlin_02 <- subset(WorldTimezones_02, timezone == "Europe/Berlin")
Berlin_01 <- subset(WorldTimezones_01, timezone == "Europe/Berlin")

pryr::object_size(Berlin)
pryr::object_size(Berlin_05)
pryr::object_size(Berlin_02)
pryr::object_size(Berlin_01)

# ----- Europe/Berline high resolution -----------------------------------------

center_x <- 6.135
center_y <- 51.145

buffer_x <- c(.005, .02, .05, .2, .5)
buffer_y <- buffer_x * sin(center_y/180 * pi)

oldPar <- par()
par(mar = c(1,1,2,1))

layout(matrix(1:6, nrow = 3, byrow = TRUE
              ))

for ( i in 1:5 ) {

  xlo <- center_x - buffer_x[i]
  xhi <- center_x + buffer_x[i]

  ylo <- center_y - buffer_y[i]
  yhi <- center_y + buffer_y[i]

  plot(Berlin, border = 'black', xlim = c(xlo, xhi), ylim = c(ylo, yhi))

  if ( i > 1 ) {
    rect(
      xleft = center_x - buffer_x[i-1],
      xright = center_x + buffer_x[i-1],
      ybottom = center_y - buffer_y[i-1],
      ytop = center_y + buffer_y[i-1],
      border = 'red'
    )
  }

  title("Europe/Berlin")

  box()

}

plot(Berlin_05, border = 'black')
rect(
  xleft = center_x - buffer_x[5],
  xright = center_x + buffer_x[5],
  ybottom = center_y - buffer_y[5],
  ytop = center_y + buffer_y[5],
  border = 'red'
)

title("Europe/Berlin")

box()

layout(1)

par(oldPar)







