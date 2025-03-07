library(conflicted)
library(tidyverse)
library(arrow)
library(sf)
library(h3jsr)
library(ggplot2)
library(h3jsr)
library(magrittr)
library(giscoR)

conflicts_prefer(dplyr::filter)

source("R/cells.R")

density <- arrow::read_parquet("data/flight_density.parquet")

res  <- 3
YYYY <- 2024
MM   <- 8
DD   <- 1
HH   <- 10

eur_hex <- bbox_nm() |>
  hexes_for_bbox_at_res(resolution = res)


ddd <- density |>
  filter(
    year == YYYY,
    month == MM,
    day == DD,
    hour == HH,
    h3_resolution == res) |>
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
  region = c("Europe", "Africa", "Asia")) |>
  mutate(res = "20M")

gisco <- gisco_RG

EUR_res20 <- gisco |>
  st_intersection(eur_hex_union) |>
  select(res)


ggplot() +
  geom_sf(data = EUR_res20,
          fill = 'lightgrey',
          linewidth = 0.1,
          # alpha = 0.5,
          colour = NA) +
  geom_sf(data = eur_hex,
          fill = NA,
          colour = 'red',
          linewidth = 0.05) +
  geom_sf(data = ddd, aes(fill = occupancy), alpha = 0.8) +
  scale_fill_viridis_c(
    trans = "log", breaks = c(1, 5, 10, 20, 30, 40, 50),
    name = "Flights per hour",
    guide = guide_legend(
      position = "top",
      keyheight = unit(3, units = "mm"),
      keywidth = unit(12, units = "mm"),
      label.position = "bottom",
      title.position = "top",
      nrow = 1
    )
  ) +
  geom_sf(data = EUR_res20, fill = NA, linewidth = 0.5, colour = "white") +
  theme_minimal() +
  coord_sf(crs = "ESRI:102013", datum = NA) +
  labs(
    title = "Hourly Flight Density",
    subtitle = str_glue("Eurocontrol Area, {YYYY}-{MM}-{DD}, Hour = {HH} UTC",
                        MM = str_pad(MM, width = 2, side = "left", pad = "0"),
                        DD = str_pad(DD, width = 2, side = "left", pad = "0"),
                        HH = str_pad(HH, width = 2, side = "left", pad = "0")),
    caption = "Data: Network Manager, EUROCONTROL"
  )

