library(conflicted)
library(tidyverse)
library(sf)
library(arrow)
library(here)
library(h3jsr)

conflict_prefer("filter", "dplyr", quiet = TRUE)

source(here("R", "cells.R"))

res <- 3
eur_hex_union <- bbox_nm() |>
  cells_boundary_at_res(resolution = res)

# circa 15 secs
# BOGUS? Just assuming that 1 position in cell is equal to (max) 30s flown
dd <- c(
  "data/trajectories_2024-08-01_resampled_30s_bbox_res_2.parquet",
  "data/trajectories_2024-12-05_resampled_30s_bbox_res_2.parquet",
  "data/trajectories_2024-08-01_resampled_30s_bbox_res_3.parquet",
  "data/trajectories_2024-12-05_resampled_30s_bbox_res_3.parquet",
  NULL) |>
  map(.f = read_parquet) |>
  bind_rows() |>
  summarise(.by = c(cell, year, month, day, hour, h3_resolution),
            occupancy = n() * 30 / 3600) |>
  arrange(desc(occupancy)) |>
  write_parquet(here("data", "flight_density.parquet"), compression = "gzip")
