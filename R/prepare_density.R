library(conflicted)
library(tidyverse)
library(sf)
library(arrow)
library(here)
library(h3jsr)

conflict_prefer("filter", "dplyr", quiet = TRUE)

# circa 15 secs
c(
  "data/trajectories_2024-08-01_resampled_30s_bbox_res_2.parquet",
  "data/trajectories_2024-12-05_resampled_30s_bbox_res_2.parquet",
  "data/trajectories_2024-08-01_resampled_30s_bbox_res_3.parquet",
  "data/trajectories_2024-12-05_resampled_30s_bbox_res_3.parquet",
  NULL) |>
  map(.f = read_parquet) |>
  bind_rows() |>
  summarise(occupancy = n() * 30 / 3600,
            .by = c(year, month, day, hour, h3_resolution, cell)) |>
  arrange(desc(occupancy)) |>
  write_parquet(here("data", "flight_density.parquet"), compression = "gzip")



# # Alternative (but far slower) computation of density
# c(
#   "data/trajectories_2024-08-01_resampled_30s_bbox_res_2.parquet",
#   "data/trajectories_2024-12-05_resampled_30s_bbox_res_2.parquet",
#   "data/trajectories_2024-08-01_resampled_30s_bbox_res_3.parquet",
#   "data/trajectories_2024-12-05_resampled_30s_bbox_res_3.parquet",
#   NULL) |>
#   purrr::map(.f = read_parquet) |>
#   dplyr::bind_rows() |>
#   dplyr::group_by(year, month, day, hour, h3_resolution, cell, flight_id) |>
#   dplyr::arrange(timestamp) |>
#   dplyr::mutate(
#     sequence_id_next = lead(sequence_id),
#     timestamp_next   = lead(timestamp),
#     is_consecutive   = if_else(is.na(sequence_id_next),
#                                FALSE,
#                                (sequence_id_next - sequence_id) < 1),
#     duration_segment = if_else(is_consecutive,
#                                (timestamp_next - timestamp) |> as.duration(),
#                                seconds(30) |> as.duration())
#     ) |>
#   ungroup() |>
#   group_by(cell, hour) |>
#   summarise(density = sum(duration_segment) / 3600)
