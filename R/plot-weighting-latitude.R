library(tidyverse)
library(aviodebris)


weightings_half_degree_latitude |>
  ggplot() +
  geom_line(aes(x = lat, y = val), linewidth = 1.3) +
  scale_x_continuous("latitude", breaks = c(-90, -60, -30, 0, 30, 60, 90)) +
  theme_minimal(base_size = 24) +
  theme(
    axis.title.y = element_blank(),
    panel.grid.minor.x = element_blank(),
  )

ggsave("media/figures/weighting.png")


# plot density
library(sf)
library(h3jsr)
library(giscoR)
library(scales)

europe_hex <- function(res_h3, res_spatial = "20M") {
  eur_hex_union <- aviodebris::bbox_nm() |>
    aviodebris::bbox_of_hexes_for_bbox_at_res(resolution = res_h3)

  gisco_get_countries(
    resolution = "20",
    epsg = "4326",
    spatialtype = "RG",
    region = c("Europe", "Africa", "Asia")
  ) |>
    mutate(res = res_spatial) |>
    st_intersection(eur_hex_union) |>
    select(res)
}

res <- 3L

eur_hex <- bbox_nm() |> hexes_for_bbox_at_res(resolution = res)

ddd <- weightings_h3_resolution_3_hourly |>
  filter(h3_resolution_3 %in% (eur_hex |> pull(h3_address))) |>
  cell_to_polygon(simple = FALSE)

MAX <- ddd |> pull("w") |> max()
min <- ddd |> pull("w") |> min()

ddd |>
  ggplot() +
  ggplot2::geom_sf(
    aes(fill = w),
    linewidth = 0.3
  ) +
  labs(
    title = "weightings on the spherical Earth",
    subtitle = "H3 hexes (res = 3)"
  )


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

EUR_res20 <- europe_hex(res_h3 = 3L)

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
    aes(fill = w),
    alpha = 0.8,
    key_glyph = draw_key_hex,
    # key_glyph = draw_key_cust
  ) +
  scale_fill_viridis_c(
    transform = "log10",
    breaks = seq(min, MAX, length.out = 4),
    # breaks = c(2e-9, 2e-8, 2e-7, 2e-6, 2e-5, 2e-4),
    name = "",
    na.value = "transparent",
    labels = label_scientific(),
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
  theme(
    legend.position = c(0.15, 0.79),
    legend.text = element_text(size = 24, face = "plain"),
    NULL
  ) +
  guides(fill = guide_legend(override.aes = list(size = 10.0)))


ggsave("media/figures/weighting_on_Earth.png")
