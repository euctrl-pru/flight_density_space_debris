# pipeline for the paper

library(tidyverse)
library(aviodebris)

busy_2023 <- seq(as.Date("2023-07-03"), by = "day", length.out = 7)
busy_2024 <- seq(as.Date("2024-07-01"), by = "day", length.out = 7)
days <- c(
  busy_2023,
  as.Date("2023-09-01"),
  busy_2024
)

# cherry peek days
# days <- c(
#   "2023-07-07",
#   "2023-09-01",
#   "2024-07-05"
# )

# exporting data 3min ca per day

# 1. resample trajectories at 30s
resample_30s <- purrr::partial(resample_traffic, interval = 30L)

# fmt: skip
{
  # tictoc::tic()
  days |> purrr::walk(.f = resample_30s)
  # tictoc::toc()
}

# 2. assign H3 hex cells at resolution 3
hexagonize_3_30s <- purrr::partial(
  hexagonize_traffic,
  resolution = 3L,
  interval = 30L
)
days |> purrr::walk(hexagonize_3_30s)

# 3. compute traffic density
traffic_density_3_30s <- purrr::partial(
  traffic_density_hourly,
  resolution = 3L,
  interval = 30L
)
days |> purrr::walk(traffic_density_3_30s)

# 4. compute casuality
casualty_3_30s_hourly <- purrr::partial(
  collision_and_casualty_risk_expectation_hourly,
  resolution = 3L
)
days |> purrr::walk(casualty_3_30s_hourly)
