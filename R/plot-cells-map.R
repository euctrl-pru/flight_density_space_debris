# H3 hexagons covering NM area bbox
library(tidyverse)
library(aviodebris)
library(pruatlas)
library(ggplot2)
library(sf)
library(here)

ms <- "BI|E.|L.|UD|UG|GM|UK|GC"
fir_ctry <- country_fir(
  firs = pruatlas::firs_nm_406,
  icao_id = "BI|E.|L.|UD|UG|GM|UK|GC",
  fl = 200,
  merge = TRUE
) |>
  sf::st_transform(crs = sf::st_crs(3035))

bbox <- fir_ctry |>
  sf::st_convex_hull() |>
  sf::st_buffer(1000) |>
  sf::st_transform(crs = sf::st_crs(4326)) |>
  sf::st_bbox()


res <- 3
g <- plot_hexes_map(res) +
  geom_sf(
    data = fir_ctry,
    fill = "blue",
    colour = "blue",
    linewidth = 0.6,
    alpha = 0.1
  ) +
  ggplot2::coord_sf(crs = "ESRI:102013", datum = NA)

ggsave(
  filename = here("media", "figures", str_glue("cells_{res}.png")),
  plot = g
)


res <- 2
g <- plot_hexes_map(res) +
  geom_sf(
    data = fir_ctry,
    fill = "blue",
    colour = "blue",
    linewidth = 0.6,
    alpha = 0.1
  ) +
  ggplot2::coord_sf(crs = "ESRI:102013", datum = NA)

ggsave(
  filename = here("media", "figures", str_glue("cells_{res}.png")),
  plot = g
)
