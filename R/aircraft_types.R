library(tidyverse)

ac_types <- c(
  "data/ectrl_acts.csv",
  "data/sky_acts.csv",
  "data/doc8643_acts.csv"
) |>
  purrr::map(.f = function(fn) {
    # fmt: skip
    cols <- cols(
      icao              = col_character(),
      name              = col_character(),
      manufacturer      = col_character(),
      type              = col_character(),
      wtc               = col_character(),
      recat_eu          = col_character(),
      mtow_kg           = col_double(),
      cruise_tas_kt     = col_double(),
      cruise_mach       = col_double(),
      cruise_range_nm   = col_double(),
      cruise_ceiling_fl = col_double(),
      wing_span_m       = col_double(),
      length_m          = col_double(),
      height_m          = col_double(),
      pax               = col_double()
    )
    read_csv(fn, col_types = cols)
  }) |>
  bind_rows()

ac_types |>
  write_csv("data/aircraft_type_info.csv")
