# scrape aircraft dimensions
library(conflicted)
library(tidyverse)
library(purrr)
library(arrow)
library(aviodebris)
library(polite)

# library(sf)
# library(here)
# library(rlang)

conflict_prefer("filter", "dplyr", quiet = TRUE)

# circa 15 secs
trjs <- c(
  "data/trajectories_2024-08-01_resampled_30s_bbox_res_2.parquet",
  "data/trajectories_2024-12-05_resampled_30s_bbox_res_2.parquet",
  "data/trajectories_2024-08-01_resampled_30s_bbox_res_3.parquet",
  "data/trajectories_2024-12-05_resampled_30s_bbox_res_3.parquet",
  NULL
) |>
  map(.f = read_parquet) |>
  bind_rows()

ac_types <- trjs |> pull(aircraft_type) |> unique() |> sort() |> setdiff("ZZZZ")


# ---- EUROCONTROL Perfromance DB ----
host <- "https://contentzone.eurocontrol.int/"
session <- polite::bow(host, force = TRUE)
scraper <- purrr::partial(scrape_aircraft_type_info_perfdb, session = session)

# some aircraft types will be missing, i.e. helicopters
all_acts <- ac_types |>
  map(.f = scraper) |>
  bind_rows()

source("R/act_passengers.R")

all_acts_missing <- all_acts |>
  filter(if_all(c(-icao), is.na)) |>
  pull(icao)

all_acts <- all_acts |>
  filter(!icao %in% all_acts_missing) |>
  mutate(across(
    c(
      "mtow_kg",
      "cruise_tas_kt",
      "cruise_mach",
      "cruise_range_nm",
      "cruise_ceiling_fl"
    ),
    as.numeric
  )) |>
  left_join(act_all_pax) |>
  mutate(pax = crew + pax_max) |>
  select(-c("accomodation", "crew", "pax_min", "pax_typical", "pax_max")) |>
  write_csv("data/ectrl_acts.csv")


# ---- SkyBrary aircraft pages ----
host <- "https://skybrary.aero/"
session <- polite::bow(host, force = TRUE)
scraper <- purrr::partial(scrape_aircraft_type_info_skybrary, session = session)


# scrape only missing type from perf DB
sky_acts <- all_acts_missing |>
  map(.f = scraper) |>
  bind_rows()

missing_sky_acts <- sky_acts |>
  filter(if_all(c(-icao), is.na)) |>
  pull(icao)

sky_acts |>
  filter(!icao %in% missing_sky_acts) |>
  left_join(sky_pax) |>
  mutate(pax = crew + pax_max) |>
  select(-c("accomodation", "crew", "pax_min", "pax_typical", "pax_max")) |>
  write_csv("data/sky_acts.csv")

# ---- doc8643.com ----
host <- "https://doc8643.com/"
session <- polite::bow(host, force = TRUE)
scraper <- purrr::partial(scrape_aircraft_type_info_doc8643, session = session)

doc8643_acts <- missing_sky_acts |>
  map(.f = scraper) |>
  bind_rows() |>
  mutate(
    type = case_when(
      icao == "C700" ~ "L2J",
      icao == "C68A" ~ "L2J",
      .default = .data$type
    )
  ) |>
  separate_wider_delim(
    manufacturer,
    " ",
    names = c("manufacturer", "other"),
    too_many = "drop"
  ) |>
  mutate(
    manufacturer = case_when(
      manufacturer == "AIR" ~ str_c(manufacturer, other, sep = " "),
      manufacturer == "AVIONES" ~ str_c(manufacturer, other, sep = " "),
      manufacturer == "GULFSTREAM" ~ str_c(manufacturer, other, sep = " "),
      other == "HELICOPTERS" & manufacturer == "AIRBUS" ~
        str_c(manufacturer, other, sep = " "),
      other == "GRUMMAN" & manufacturer == "NORTHROP" ~
        str_c(manufacturer, other, sep = " "),
      other == "MARTIN" & manufacturer == "LOCKHEED" ~
        str_c(manufacturer, other, sep = " "),
      .default = manufacturer
    )
  ) |>
  select(-other)

doc8643_acts |>
  rename(pax = accomodation) |>
  write_csv("data/doc8643_acts.csv")
