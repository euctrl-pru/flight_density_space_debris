library(conflicted)
library(tidyverse)
library(aviodebris)
library(arrow)
library(sf)
library(h3jsr)
library(ggplot2)
library(h3jsr)
library(magrittr)
library(giscoR)
library(grid)

conflicts_prefer(dplyr::filter)


density <- arrow::read_parquet("data/flight_density.parquet")

res <- 3
YYYY <- 2024
MM <- 8
DD <- 1
HH <- 10

eur_hex <- bbox_nm() |>
  hexes_for_bbox_at_res(resolution = res)


ddd <- density |>
  filter(
    year == YYYY,
    month == MM,
    day == DD,
    hour == HH,
    h3_resolution == res
  ) |>
  mutate(occupancy = mean(occupancy), .by = cell) |>
  filter(cell %in% (eur_hex |> pull(h3_address))) |>
  select(cell, everything()) |>
  cell_to_polygon(simple = FALSE)

MAX <- ddd |> pull("occupancy") |> max()
min <- ddd |> pull("occupancy") |> min()
min <- 0


eur_hex_union <- bbox_nm() |>
  bbox_of_hexes_for_bbox_at_res(resolution = res)

gisco_RG <- gisco_get_countries(
  resolution = "20",
  epsg = "4326",
  spatialtype = "RG",
  region = c("Europe", "Africa", "Asia")
) |>
  mutate(res = "20M")

gisco <- gisco_RG

EUR_res20 <- gisco |>
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

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

# Copy & Paste from ggplot2::draw_key_rect
draw_key_cust <- function(data, params, size) {
  # make fill inherit color if NA
  if (is.na(data$fill)) data$fill <- data$col

  grid::rectGrob(
    gp = grid::gpar(
      col = NA,
      fill = alpha(
        data$fill %||%
          data$colour %||%
          "grey20",
        data$alpha
      ),
      lty = data$linetype %||%
        1
    )
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
    # key_glyph = draw_key_cust
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
  geom_sf(data = EUR_res20, fill = NA, linewidth = 0.65, colour = "white") +
  # annotate() +
  theme_minimal() +
  coord_sf(crs = "ESRI:102013", datum = NA) +
  labs(
    title = "Hourly Flight Density",
    subtitle = str_glue(
      "EUROCONTROL Area, {YYYY}-{MM}-{DD}, Hour = {HH} UTC",
      MM = str_pad(MM, width = 2, side = "left", pad = "0"),
      DD = str_pad(DD, width = 2, side = "left", pad = "0"),
      HH = str_pad(HH, width = 2, side = "left", pad = "0")
    ),
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
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 13),
    NULL
  ) +
  guides(fill = guide_legend(override.aes = list(size = 3.0)))

ggsave("media/figures/flight_density_2024-08-01T12_3.png")
