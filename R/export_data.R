# point profile for 1st Aug

library(conflicted)
library(tidyverse)
library(eurocontrol)
library(arrow)
library(sf)
library(giscoR)
library(sf)
library(smoothr)
library(tidyverse)
library(here)
library(ggplot2)
library(h3jsr)
library(magrittr)

withr::local_envvar(c(TZ = "UTC",
                      ORA_SDTZ = "UTC",
                      NLS_LANG = ".AL32UTF8"))
conn <- withr::local_db_connection(db_connection("PRU_DEV"))

source(here("R", "cells.R"))

# bbox Europe/EUROCONTROL area
# -25.488281,26.638253,45.407181,71.864780
bb_nm <- bbox_nm() |>
  st_bbox(crs = 4326) |>
  st_as_sfc(crs = 4326) |>
  # add points to the bbox polygon
  densify(n = 300L) |>
  st_as_sf(res = 2)



res <- 2
eur_hex <- bbox_nm() |> hexes_for_bbox_at_res(resolution = res)

# take the bbox of the union of hexes at resolution 2
# as the bbox for the query
eur_hex_union <- bbox_nm() |> bbox_of_hexes_for_bbox_at_res(resolution = res)
bb <- eur_hex |> st_bbox()


# Aug 2024
wef <- "2024-08-01" |> as_date()
til <- wef + ddays(1)

trjs <- point_profiles_tidy(conn = conn,
                    wef = wef,
                    til = til,
                    bbox = bb,
                    profile = "CTFM")

trjs |>
  mutate(altitude = 100 * FLIGHT_LEVEL) |>
  select(flight_id     = FLIGHT_ID,
         callsign      = CALLSIGN,
         icao24        = ICAO24,
         aircraft_type = AIRCRAFT_TYPE,
         timestamp     = TIME_OVER,
         longitude     = LONGITUDE,
         latitude      = LATITUDE,
         altitude,
         sequence_id   = SEQ_ID) |>
  collect() |>
  write_parquet(str_glue("data-raw/trjs_{dddd}.parquet",
                         dddd = format(wef, "%Y-%m-%d")))


# Dec 2024
wef <- "2024-12-05" |> as_date()
til <- wef + ddays(1)

trjs <- point_profiles_tidy(conn = conn,
                            wef = wef,
                            til = til,
                            bbox = bb,
                            profile = "CTFM")

trjs |>
  mutate(altitude = 100 * FLIGHT_LEVEL) |>
  select(flight_id     = FLIGHT_ID,
         callsign      = CALLSIGN,
         icao24        = ICAO24,
         aircraft_type = AIRCRAFT_TYPE,
         timestamp     = TIME_OVER,
         longitude     = LONGITUDE,
         latitude      = LATITUDE,
         altitude,
         sequence_id   = SEQ_ID) |>
  collect() |>
  write_parquet(str_glue("data-raw/trjs_{dddd}.parquet",
                         dddd = format(wef, "%Y-%m-%d")))
