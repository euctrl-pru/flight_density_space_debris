library(conflicted)
library(tidyverse)
library(aviodebris)
library(arrow)
library(sf)
library(h3jsr)
library(ggplot2)
library(magrittr)
library(giscoR)
library(grid)
library(here)

conflicts_prefer(dplyr::filter)

date <- "2024-07-05" |> as_date()
density <- arrow::read_parquet(
  str_glue("data/traffic_density_{date}_res_3_hourly.parquet")
)

resolution <- 3

eur_hex <- bbox_nm() |>
  hexes_for_bbox_at_res(resolution = resolution)


ddd <- density |>
  group_by(cell) |>
  summarise(occupancy = sum(occupancy) / 24) |>
  filter(cell %in% (eur_hex |> pull(h3_address))) |>
  select(cell, everything()) |>
  cell_to_polygon(simple = FALSE)

MAX <- ddd |> pull("occupancy") |> max()
min <- ddd |> pull("occupancy") |> min()
min <- 0


eur_hex_union <- bbox_nm() |>
  bbox_of_hexes_for_bbox_at_res(resolution = resolution)

gisco_RG <- gisco_get_countries(
  resolution = "20",
  epsg = "4326",
  spatialtype = "RG",
  region = c("Europe", "Africa", "Asia")
) |>
  mutate(res = "20M")


EUR_res20 <- gisco_RG |>
  st_intersection(eur_hex_union) |>
  select(res)

# draw hexagon for key shape in legend
draw_key_hex <- function(data, params, size) {
  # make fill inherit color if NA
  if (is.na(data$fill)) data$fill <- data$col

  # hexagon vertex coordinates
  v <- list(
    x = c(0.95, 0.725, 0.275, 0.05, 0.275, 0.725),
    y = c(
      0.5,
      0.110288568297003,
      0.110288568297003,
      0.5,
      0.889711431702997,
      0.889711431702997
    )
  )
  # hexagon grob
  grid::polygonGrob(
    v$x,
    v$y,
    gp = grid::gpar(col = data$colour, fill = alpha(data$fill, data$alpha))
  )
}


ggplot() +
  geom_sf(
    data = eur_hex,
    fill = "#7DF9FF",
    colour = NA,
    # linewidth = 0.05,
    NULL
  ) +
  # scale_fill_manual(values = c("#7DF9FF")) +
  geom_sf(
    data = EUR_res20,
    fill = 'lightgrey',
    # linewidth = 0.1,
    # alpha = 0.5,
    # colour = "black",
    colour = NA,
    NULL
  ) +
  geom_sf(data = eur_hex, fill = NA, colour = 'red', linewidth = 0.05) +
  geom_sf(
    data = ddd,
    aes(fill = occupancy),
    alpha = 0.8,
    key_glyph = draw_key_hex
  ) +
  scale_fill_viridis_c(
    trans = "log",
    breaks = c(0.1, 1, 5, 10, 20, 30, 40, 50),
    name = "Flights/hour",
    na.value = "transparent",
    guide = guide_legend(
      # position = "left",
      keyheight = unit(3, units = "mm"),
      keywidth = unit(3, units = "mm"),
      label.position = "bottom",
      title.position = "top",
      nrow = 1
    )
  ) +
  # highlight MAX
  geom_sf(
    data = ddd |> mutate(MAX = max(occupancy)) |> filter(occupancy == MAX),
    fill = "black",
    alpha = 0.8,
    key_glyph = draw_key_hex
  ) +
  geom_sf(data = EUR_res20, fill = NA, linewidth = 0.45, colour = "white") +
  # annotate() +
  theme_minimal() +
  coord_sf(crs = "ESRI:102013", datum = NA) +
  labs(
    caption = "Data: Network Manager, EUROCONTROL",
    NULL
  ) +
  theme(
    plot.title = element_text(
      # hjust = 0.09,
      size = 15,
      margin = margin(t = 0, r = 0, b = 2, l = 0, unit = "mm")
    ),
    plot.subtitle = element_text(
      size = 12,
      # hjust = 0.128,
      # face = "bold",
      margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "mm")
    ),
    plot.caption = element_text(
      size = 13,
      # hjust = 0.5,
      # face = "bold",
      margin = margin(t = -5, r = 10, b = 0, l = 0, unit = "mm")
    ),
    legend.position = c(0.18, 0.8),
    legend.title = element_text(size = 24),
    legend.text = element_text(size = 24),
    NULL
  ) +
  guides(fill = guide_legend(override.aes = list(size = 3.0)))

ggsave(
  # width = 80,
  # units = "mm",
  dpi = 600,
  here(
    "media",
    "figures",
    str_glue("flight_density_{date}_{resolution}.png")
  )
)
