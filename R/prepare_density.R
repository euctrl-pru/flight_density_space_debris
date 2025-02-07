library(conflicted)
library(tidyverse)
library(sf)
library(arrow)
library(here)

conflict_prefer("filter", "dplyr", quiet = TRUE)

source(here("R", "cells.R"))

res <- 2
eur_hex_union <- bbox_nm() |> cells_boundary_at_res(resolution = res)


dd <- c(
  "data/trajectories_2024-08-01_resampled.parquet",
  # "data/trajectories_2024-12-05_resampled.parquet",
  NULL
  ) |>
  read_parquet() |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_intersection(eur_hex_union)

dd |>
  point_to_cell(res = 2, simple = FALSE) |>
  as_tibble() |>
  rename(cell = h3_resolution_2) |>
  summarise(.by = c(cell, hour), occupancy = n() * 30 / 3600) |>
  arrange(desc(occupancy)) |>
  write_parquet(here("data", "flight_density.parquet"), compression = "gzip")
