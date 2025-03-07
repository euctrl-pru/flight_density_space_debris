# check frequency of points: 30s?
# NOT homogeneous!

library(conflicted)
library(tidyverse)
library(arrow)

conflicts_prefer(dplyr::filter)

trj <- read_parquet('data/trajectory_2024-08-01.parquet')

occupancy <- trj |>
  arrange(TIME_OVER) |>
  mutate(
    delta = lead(TIME_OVER)- TIME_OVER,
    .by = c("FLIGHT_ID", "CELL", "HOUR")) |>
  filter(!is.na(DELTA))

trj |>
  # duration of each segment inside a CELL in a given HOUR
  mutate(
    DELTA = lead(TIME_OVER)- TIME_OVER,
    .by = c("FLIGHT_ID", "CELL_ID", "HOUR")) |>
  filter(!is.na(DELTA)) |>
  group_by(FLIGHT_ID, CELL_ID, HOUR) |>
  summarise(occupancy = sum(delta)) |>
  arrange(FLIGHT_ID, desc(CELL_ID), HOUR)

# this proves that TIME_OVER rates are not at 30s for a single flight
trj |>
  filter(FLIGHT_ID == 274761935,
         HOUR == 0) |>
  arrange(TIME_OVER) |>
  mutate(DELTA = lead(TIME_OVER) - TIME_OVER) |>
  filter(!is.na(DELTA))






library(conflicted)
library(duckdb)
library(tidyverse, warn.conflicts = FALSE)

conflict_prefer("filter", "dplyr", quiet = TRUE)
conflict_prefer("lag", "dplyr")
conflict_prefer("lead", "dplyr")


## Instantiate the in-memory DuckDB connection
con = dbConnect(duckdb(), shutdown = TRUE)

## Register our parquet dataset as table in our connection (and that assign it
## to an object that R understands)
trj = tbl(con, "read_parquet('data/trajectory_2024-08-01.parquet')")


trj |>
  group_by(CELL, HOUR, FLIGHT_ID) |>
  mutate(NEXT = lead(TIME_OVER)) |>
  mutate(DELTA = NEXT - TIME_OVER)
  # duration of each segment inside a CELL in a given HOUR
  mutate(
    DELTA = lead(TIME_OVER)- TIME_OVER,
    .by = c("FLIGHT_ID", "CELL", "HOUR")) |>
  filter(!is.na(DELTA)) |>
  group_by(FLIGHT_ID, CELL_ID, HOUR) |>
  summarise(occupancy = sum(delta)) |>
  arrange(FLIGHT_ID, desc(CELL_ID), HOUR)
