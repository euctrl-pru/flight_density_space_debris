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
library(latex2exp)

conflicts_prefer(dplyr::filter)

resolution <- 3
date <- "2024-07-05" |> as_date()

# longitudes at half degrees
lons <- seq(0L, 719L) / 2
hs <- 24

# Earth equatorial radius
rE <- 6378e3
# altitude of satellite from (spherical) Earth surface, i.e. 500 km for LEO
alt <- 550e3

# inclination of Kosmos 482 Descent Craft (1972-023E, cat. nr. 6073)
# see https://sattrackcam.blogspot.com/2025/04/kosmos-842-descent-craft-reentry.html
inclination_deg = 51.95

weightings_h3_resolution_3_hourly_kosmos <- latitude_weights(
  delta_lat = 0.5,
  altitude = rE + alt,
  inclination_deg = inclination_deg
) |>
  mutate(lon = list(lons)) |>
  unnest(lon) |>
  mutate(
    # spread by longitude
    val = val / length(lons),
    # spread by hour
    val = val / hs,
    NULL
  ) |>
  relocate(lon, .before = lat) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  point_to_cell(res = resolution, simple = FALSE) |>
  # integrate (sum) over the ones in each cell and divide by the cell area
  # (take the mean of all the values at half degree)
  summarize(
    w = mean(val),
    .by = h3_resolution_3
  ) |>
  as_tibble()


collision <- here::here(
  "data",
  stringr::str_glue(
    "traffic_density_{date}_res_{resolution}_hourly.parquet"
  )
) |>
  arrow::read_parquet() |>
  dplyr::filter(.data$h3_resolution == resolution) |>
  dplyr::left_join(
    weightings_h3_resolution_3_hourly_kosmos,
    by = c("cell" = stringr::str_glue("h3_resolution_{resolution}"))
  ) |>
  # per cell per hour per aircraft type
  dplyr::group_by(
    .data$year,
    .data$month,
    .data$day,
    .data$hour,
    .data$cell,
    .data$aircraft_type
  ) |>
  dplyr::summarise(
    # take the mean of the half degree values
    w = mean(.data$w),
    # just take the mean, but values should all be the same
    # this is done to keep the column
    density_m2 = mean(.data$density_m2)
  ) |>
  dplyr::ungroup() |>
  dplyr::left_join(
    aviodebris::effective_expose_area,
    by = c("aircraft_type" = "icao")
  ) |>
  dplyr::mutate(
    eea = dplyr::if_else(is.na(.data$eea), 500, .data$eea),
    pax = dplyr::if_else(is.na(.data$pax), 7, .data$pax)
  ) |>
  # per cell per hour per aircraft type
  dplyr::group_by(
    .data$year,
    .data$month,
    .data$day,
    .data$hour,
    .data$cell,
    .data$aircraft_type
  ) |>
  dplyr::summarize(
    collision_expectation = .data$w * .data$density_m2 * .data$eea
  ) |>
  dplyr::ungroup() |>
  # daily now
  dplyr::group_by(.data$year, .data$month, .data$day, .data$cell) |>
  dplyr::summarise(collision_expectation = sum(collision_expectation)) |>
  dplyr::ungroup()


eur_hex <- bbox_nm() |>
  hexes_for_bbox_at_res(resolution = resolution)


ddd <- collision |>
  filter(cell %in% (eur_hex |> pull(h3_address))) |>
  relocate(cell, .before = 1) |>
  cell_to_polygon(simple = FALSE)

MAX <- ddd |> pull("collision_expectation") |> max()
min <- ddd |> pull("collision_expectation") |> min()

collision |>
  filter(cell %in% (eur_hex |> pull(h3_address))) |>
  relocate(cell, .before = 1) |>
  cell_to_polygon(simple = FALSE) |>
  mutate(max_collision = max(collision_expectation)) |>
  filter(collision_expectation == max_collision)

eur_hex_union <- bbox_nm() |>
  bbox_of_hexes_for_bbox_at_res(resolution = resolution)

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


library(scales)
transform_log10p1 <- new_transform(
  name = "log10p1",
  transform = function(x) log10(x + 1),
  inverse = function(x) (10^x) - 1,
  breaks = log_breaks(n = 4, base = 10)
)


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
    aes(fill = collision_expectation),
    alpha = 0.8,
    key_glyph = draw_key_hex
  ) +
  scale_fill_viridis_c(
    # trans = transform_log10p1,
    trans = "pseudo_log",
    breaks = seq(min, MAX, length.out = 4) |>
      format(digits = 3) |>
      as.numeric(),
    name = unname(TeX("$E_{h}$")),
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
  theme_minimal() +
  coord_sf(crs = "ESRI:102013", datum = NA) +
  theme(
    plot.caption = element_text(
      size = 13,
      margin = margin(t = -5, r = 10, b = 0, l = 0, unit = "mm")
    ),
    legend.position = c(0.18, 0.8),
    legend.title = element_text(size = 24),
    legend.text = element_text(size = 24),
    NULL
  ) +
  guides(fill = guide_legend(override.aes = list(size = 3.0)))

ggsave(here(
  "media",
  "figures",
  str_glue("collision_expectation_{date}_{resolution}_kosmos.png")
))
