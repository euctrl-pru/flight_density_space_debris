# H3 hexagons covering NM area bbox

library(giscoR)
library(sf)
library(smoothr)
library(tidyverse)
library(here)

source("R/cells.R")



library(ggplot2)
library(h3jsr)
library(magrittr)

res <- 3

# bbox Europe/EUROCONTROL area
# -25.488281,26.638253,45.407181,71.864780

bb <- bbox_nm() |>
  st_bbox(crs = 4326) |>
  st_as_sfc(crs = 4326) |>
  # add points to the bbox polygon
  densify(n = 300L) |>
  st_as_sf(res = res)


cells <- bb |>
  st_transform(crs = 4326) |>
  polygon_to_cells(res = res, simple = FALSE)

eur_hex <- cells |>
  pull(h3_addresses) |>
  unlist() |>
  cell_to_polygon(simple = FALSE)

eur_hex_union <- eur_hex |>
  st_union() |>
  st_exterior_ring() |>
  st_as_sf() |>
  mutate(resolution = res)

eur_centreoid <- cells |>
  pull(h3_addresses) |>
  unlist() |>
  cell_to_point(simple = FALSE)

gisco_RG <- gisco_get_countries(
  resolution = "20",
  epsg = "4326",
  spatialtype = "RG",
  region = c("Europe", "Africa", "Asia")) |>
  mutate(res = "20M")

gisco <- gisco_RG

EUR_res20 <- gisco |>
  st_intersection(eur_hex_union) |>
  select(res)




eu = st_geometry(st_normalize(st_as_sf(EUR_res20)))

g <- ggplot() +
  geom_sf(data = EUR_res20, fill = 'lightgrey', linewidth = 0.4) +
  geom_sf(data = eur_hex, fill = NA, colour = 'red', linewidth = 0.3) +
  geom_sf(data = eur_centreoid, fill = NA, colour = 'blue', size = 0.1) +
  theme_minimal() +
  coord_sf(crs = "ESRI:102013", datum = NA)
g

ggsave(filename = here("figures", str_glue("cells-{res}.png")), plot = g)
