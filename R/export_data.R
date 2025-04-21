# export point profiles for selected days

library(conflicted)
library(withr)
library(eurocontrol)
library(tidyverse)
library(arrow)
library(here)
library(aviodebris)
library(sf)

busy_2023 <- seq(as.Date("2023-07-03"), by = "day", length.out = 7)
busy_2024 <- seq(as.Date("2024-07-01"), by = "day", length.out = 7)

days <- c(
  busy_2023,
  as.Date("2023-09-01"),
  busy_2024
)

days_plus_1 <- days + lubridate::ddays(1)

withr::local_envvar(c(TZ = "UTC", ORA_SDTZ = "UTC", NLS_LANG = ".AL32UTF8"))
conn <- withr::local_db_connection(db_connection("PRU_DEV"))

# bbox Europe/EUROCONTROL area
resolution <- 3
bbox <- aviodebris::bbox_nm() |>
  # take the bbox of the union of hexes at resolution 3
  aviodebris::hexes_for_bbox_at_res(resolution = resolution) |>
  sf::st_bbox()

export_profile <- purrr::partial(
  point_profiles_tidy,
  conn = conn,
  profile = "CTFM",
  bbox = bbox
)


purrr::walk2(days, days_plus_1, \(wef, til) {
  export_profile(wef = wef, til = til) |>
    dplyr::mutate(altitude = 100 * FLIGHT_LEVEL) |>
    # fmt: skip
    dplyr::select(
      flight_id     = FLIGHT_ID,
      callsign      = CALLSIGN,
      icao24        = ICAO24,
      aircraft_type = AIRCRAFT_TYPE,
      timestamp     = TIME_OVER,
      longitude     = LONGITUDE,
      latitude      = LATITUDE,
      altitude,
      sequence_id   = SEQ_ID
    ) |>
    dplyr::collect() |>
    arrow::write_parquet(
      here(
        "data-raw",
        "trjs",
        stringr::str_glue("trjs_{dddd}.parquet", dddd = format(wef, "%Y-%m-%d"))
      )
    )
})
