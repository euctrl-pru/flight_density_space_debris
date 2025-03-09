# adapted from Fig 6.9 (and relevant R code) in
# https://r-spatial.org/book/06-Cubes.html#sec-switching

set.seed(1331)
library(here)
library(stars)
library(colorspace)
tif <- system.file("tif/L7_ETMs.tif", package = "stars")
r <- read_stars(tif)


nrow <- 2
ncol <- 8
m <- matrix(runif(nrow * ncol), nrow = nrow, ncol = ncol)
m <- r[[1]][1:nrow, 1:ncol, 1]
dim(m) <- c(x = nrow, y = ncol) # named dim
s <- st_as_stars(m)
# s
attr(s, "dimensions")[[1]]$delta = 3
attr(s, "dimensions")[[2]]$delta = -.5
attr(attr(s, "dimensions"), "raster")$affine = c(-1.2, 0.0)

# plot plate
plt <- function(x, yoffset = 0, add, li = TRUE) {
  attr(x, "dimensions")[[2]]$offset = attr(x, "dimensions")[[2]]$offset +
    yoffset
  l = st_as_sf(x, as_points = FALSE)
  pal = sf.colors(10)
  if (li) pal = lighten(pal, 0.3 + rnorm(1, 0, 0.1))
  if (!add)
    plot(
      l,
      axes = FALSE,
      breaks = "equal",
      pal = pal,
      reset = FALSE,
      border = grey(.75),
      key.pos = NULL,
      main = NULL,
      xlab = "hour"
    ) else
    plot(
      l,
      axes = TRUE,
      breaks = "equal",
      pal = pal,
      add = TRUE,
      border = grey(.75)
    )
  u = st_union(l)
  plot(st_geometry(u), add = TRUE, col = NA, border = 'black', lwd = 2.5)
}

# plot the cube
pl <- function(s, x, y, add = TRUE, randomize = FALSE) {
  attr(s, "dimensions")[[1]]$offset = x
  attr(s, "dimensions")[[2]]$offset = y
  m = r[[1]][y + 1:nrow, x + 1:ncol, 1]
  if (randomize) m = m[sample(y + 1:nrow), x + 1:ncol]
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
library(spacetime)
data(air)
de = st_geometry(st_normalize(st_as_sf(DE)))

library(h3jsr)
source("R/cells.R")
res <- 3
eur_hex <- DE |>
  st_bbox() |>
  hexes_for_bbox_at_res(resolution = res)

eur_centreoid <- eur_hex |>
  dplyr::pull(h3_address) |>
  unlist() |>
  cell_to_point(simple = FALSE) |>
  st_normalize() |>
  st_geometry()
eur_centreoid <- eur_centreoid * 6 + c(-8.5, 8.5)

eur_hex <- eur_hex |>
  st_normalize() |>
  st_geometry()
eur_hex <- eur_hex * 6 + c(-8.8, 8.5)

de = de * 6 + c(-8.5, 8.5)

# pdf(here("media", "figures", "cubes.pdf"))
png(
  filename = here("media", "figures", "cubes.png"),
  width = 19,
  height = 18.5,
  units = "cm",
  res = 600
)

plot.new()
par(mar = c(0, 0, 0, 0), cex = 1.5)
plot.window(xlim = c(-11, 8), ylim = c(-4, 14.5), asp = 1)
#
pl(s, 0, 0, TRUE, randomize = TRUE)
plot(de, add = TRUE, border = grey(.5))
plot(eur_hex, add = TRUE, border = grey(.8))
text(-11, 0, "hour", srt = -90, col = 'black')
text(3.3, 9.5, "densities", srt = 0, col = 'black')
text(1.45, 8.6, expression(rho[act]), col = 'black')
text(4.45, 8.6, expression(rho[pop]), col = 'black')
# hours
text(-10.1, 4, "23", srt = -90, col = 'black', cex = 0.8)
text(-10.1, 3, "22", srt = -90, col = 'black', cex = 0.8)
text(-10.1, 2, "21", srt = -90, col = 'black', cex = 0.8)
text(-10.1, 1, ".", srt = -90, col = 'black', cex = 0.8)
text(-10.1, 0, ".", srt = -90, col = 'black', cex = 0.8)
text(-10.1, -1, ".", srt = -90, col = 'black', cex = 0.8)
text(-10.1, -2, "2", srt = -90, col = 'black', cex = 0.8)
text(-10.1, -3, "1", srt = -90, col = 'black', cex = 0.8)
text(-10.1, -4, "0", srt = -90, col = 'black', cex = 0.8)


# location points:
p <- st_coordinates(s[, 1])
p[, 1] <- p[, 1] - 1.4
p[, 2] <- p[, 2] + 8.2
points(p, col = grey(.7), pch = 16)
# centroids:
set.seed(1187)
cent <- st_coordinates(st_sample(de, 8))
cent <- st_coordinates(st_sample(st_centroid(eur_hex), 8))
points(cent, col = grey(.7), pch = 16, cex = 0.5)
cent <- cent[rev(order(cent[, 1])), ]
seg <- cbind(p, cent[1:8, ])
segments(seg[, 1], seg[, 2], seg[, 3], seg[, 4], col = 'grey')

text(-4.5, 7.1, "hex id", srt = 25, col = 'black')
dev.off()
