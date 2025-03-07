# from Fig 6.9 (and relevant R code) in
# https://r-spatial.org/book/06-Cubes.html#sec-switching

set.seed(1331)
library(stars)
library(colorspace)
tif <- system.file("tif/L7_ETMs.tif", package = "stars")
r <- read_stars(tif)

nrow <- 5
ncol <- 8
#m = matrix(runif(nrow * ncol), nrow = nrow, ncol = ncol)
m <- r[[1]][1:nrow,1:ncol,1]
dim(m) <- c(x = nrow, y = ncol) # named dim
s <- st_as_stars(m)
# s
attr(s, "dimensions")[[1]]$delta = 3
attr(s, "dimensions")[[2]]$delta = -.5
attr(attr(s, "dimensions"), "raster")$affine = c(-1.2, 0.0)

plt <- function(x, yoffset = 0, add, li = TRUE) {
  attr(x, "dimensions")[[2]]$offset = attr(x, "dimensions")[[2]]$offset + yoffset
  l = st_as_sf(x, as_points = FALSE)
  pal = sf.colors(10)
  if (li)
    pal = lighten(pal, 0.3 + rnorm(1, 0, 0.1))
  if (! add)
    plot(l, axes = FALSE, breaks = "equal", pal = pal, reset = FALSE, border = grey(.75), key.pos = NULL, main = NULL, xlab = "time")
  else
    plot(l, axes = TRUE, breaks = "equal", pal = pal, add = TRUE, border = grey(.75))
  u = st_union(l)
  plot(st_geometry(u), add = TRUE, col = NA, border = 'black', lwd = 2.5)
}

pl <- function(s, x, y, add = TRUE, randomize = FALSE) {
  attr(s, "dimensions")[[1]]$offset = x
  attr(s, "dimensions")[[2]]$offset = y
  m = r[[1]][y + 1:nrow,x + 1:ncol,1]
  if (randomize)
    m = m[sample(y + 1:nrow),x + 1:ncol]
  dim(m) = c(x = nrow, y = ncol) # named dim
  s[[1]] = m
  plt(s, 0, add)
  plt(s, 1, TRUE)
  plt(s, 2, TRUE)
  plt(s, 3, TRUE)
  plt(s, 4, TRUE)
  plt(s, 5, TRUE)
  plt(s, 6, TRUE)
  plt(s, 7, TRUE)
  plt(s, 8, TRUE, FALSE)
}

# point vector data cube:
plot.new()
par(mar = c(5, 0, 5, 0))
plot.window(xlim = c(-10, 16), ylim = c(-2,12), asp = 1)
library(spacetime)
data(air)
de = st_geometry(st_normalize(st_as_sf(DE)))
#
pl(s, 0, 0, TRUE, randomize = TRUE)
de = de * 6 + c(-7, 9)
plot(de, add = TRUE, border = grey(.5))
text(-10, 0, "time", srt = -90, col = 'black')
text(-5,  7.5, "sensor location", srt = 25, col = 'black')
text( 7,  10.5, "air quality parameter", srt = 0, col = 'black')
text( 1.5,  8.5, expression(PM[10]), col = 'black', cex = .75)
text( 4.5,  8.5, expression(NO[x]), col = 'black', cex = .75)
text( 8,  8.5, expression(SO[4]), col = 'black', cex = .75)
text( 11,  8.5, expression(O[3]), col = 'black', cex = .75)
text( 14,  8.5, expression(CO), col = 'black', cex = .75)
# location points:
p <- st_coordinates(s[,1])
p[,1] <- p[,1]-1.4
p[,2] <- p[,2] + 8.2
points(p, col = grey(.7), pch = 16)
# centroids:
set.seed(131)
cent <- st_coordinates(st_sample(de, 8))
points(cent, col = grey(.7), pch = 16)
cent <- cent[rev(order(cent[,1])),]
seg <- cbind(p, cent[1:8,])
segments(seg[,1], seg[,2], seg[,3], seg[,4], col = 'grey')
