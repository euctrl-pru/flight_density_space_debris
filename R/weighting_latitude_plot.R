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
bb <- bbox_nm()
xlim <- c(bb["xmin"], bb["xmax"])
ylim <- c(bb["ymin"], bb["ymax"])

weightings_h3_resolution_3_hourly |>
  cell_to_polygon() |>
  ggplot() +
  geom_sf(crs = st_crs("EPSG:4326")) +
  coord_sf(xlim = xlim, ylim = ylim)
