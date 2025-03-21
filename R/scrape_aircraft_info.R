# scrape aircraft dimensions
library(conflicted)
library(tidyverse)
library(sf)
library(arrow)
library(here)
library(rvest)
library(httr)
library(polite)
library(eurocontrol)

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


scrape_aircraft_type_info_perfdb <- function(ac_type, session) {
  # For some verbose status updates
  cli::cli_process_start(
    "Scrape {.val {ac_type}} at EUROCONTROL Performance DB"
  )
  # Create full url and scrape
  full_url <- polite::nod(
    session,
    str_glue(
      "aircraftperformance/details.aspx?ICAO={ac_type}",
      ac_type = toupper(ac_type)
    )
  )
  page <- full_url |> polite::scrape()

  icao <- ac_type |> toupper()
  if (
    page |> html_text2() |> str_detect("The URL you have used returns no data")
  ) {
    # fmt: skip
    response <- tibble_row(
      icao           = icao,
      name           = NA_character_,
      manufacturer   = NA_character_,
      type           = NA_character_,
      wtc            = NA_character_,
      recat_eu       = NA_character_,
      mtow_kg        = NA,
      cruise_tas_kt  = NA,
      cruise_mach    = NA,
      cruise_range_nm   = NA,
      cruise_ceiling_fl = NA,
      wing_span_m      = NA,
      length_m         = NA,
      height_m         = NA,
      accomodation   = NA_character_
    )
  } else {
    name <- page |>
      html_elements("#MainContent_wsAcftNameLabel") |>
      rvest::html_text2()

    manufacturer <- page |>
      html_elements("#MainContent_wsManufacturerLabel") |>
      rvest::html_text2()

    type <- page |>
      html_elements("#MainContent_wsTypeLabel") |>
      rvest::html_text2()

    wtc <- page |>
      html_elements("#MainContent_wsWTCLabel") |>
      rvest::html_text2()

    recat_eu <- page |>
      html_elements("#MainContent_wsRecatEULabel") |>
      rvest::html_text2()

    mtow_kg <- page |>
      html_elements("#wsMTOWLiteral") |>
      rvest::html_text2()

    cruise_tas_kt <- page |>
      html_elements("#wsVCSknotsLiteral") |>
      rvest::html_text2()

    cruise_mach <- page |>
      html_elements("#wsVCSmachLiteral") |>
      rvest::html_text2()

    cruise_range_nm <- page |>
      html_elements("#wsRangeLiteral") |>
      rvest::html_text2()

    cruise_ceiling_fl <- page |>
      html_elements("#wsCeilingLiteral") |>
      rvest::html_text2()

    wing_span_m <- page |>
      html_elements("#MainContent_wsLabelWingSpan") |>
      rvest::html_text2() |>
      str_remove(" m$") |>
      as.numeric()

    length_m <- page |>
      html_elements("#MainContent_wsLabelLength") |>
      rvest::html_text2() |>
      str_remove(" m$") |>
      as.numeric()

    height_m <- page |>
      html_elements("#MainContent_wsLabelHeight") |>
      rvest::html_text2() |>
      str_remove(" m$") |>
      as.numeric()

    accomodation <- page |>
      html_elements("#MainContent_wsLabelAccommodation") |>
      rvest::html_text2()

    # fmt: skip
    response <- tibble_row(
      icao           = icao,
      name           = name,
      manufacturer   = manufacturer,
      type           = type,
      wtc            = wtc,
      recat_eu       = recat_eu,
      mtow_kg        = mtow_kg,
      cruise_tas_kt  = cruise_tas_kt,
      cruise_mach    = cruise_mach,
      cruise_range_nm   = cruise_range_nm,
      cruise_ceiling_fl = cruise_ceiling_fl,
      wing_span_m      = wing_span_m,
      length_m         = length_m,
      height_m         = height_m,
      accomodation   = accomodation
    )
  }

  cli::cli_process_done()
  response
}


scrape_aircraft_type_info_skybrary <- function(ac_type, session) {
  make_valid <- function(value) {
    value <- ifelse(
      length(value) == 0,
      NA,
      value
    )
    value <- ifelse(
      value == "n/a",
      NA,
      value
    )
    value
  }
  # For some verbose status updates
  cli::cli_process_start("Scrape {.val {ac_type}} at SkyBrary")
  # Create full url and scrape
  full_url <- polite::nod(
    session,
    str_glue(
      "aircraft/{ac_type}",
      ac_type = tolower(ac_type)
    )
  )
  page <- full_url |> polite::scrape()

  if (is.null(page)) {
    # fmt: skip
    response <- tibble_row(
      icao           = toupper(ac_type),
      name           = NA_character_,
      manufacturer   = NA_character_,
      type           = NA_character_,
      wtc            = NA_character_,
      recat_eu       = NA_character_,
      mtow_kg        = NA,
      cruise_tas_kt  = NA,
      cruise_mach    = NA,
      cruise_range_nm   = NA,
      cruise_ceiling_fl = NA,
      wing_span_m      = NA,
      length_m         = NA,
      height_m         = NA,
      accomodation   = NA_character_
    )
  } else {
    icao <- ac_type |> toupper()

    path <- ".field-node-field-aircraft-name.field-name-field-aircraft-name.field-type-string.field-label-above.has-single > div.field-items > div"
    name <- page |>
      html_elements(path) |>
      rvest::html_text2()

    path <- ".field-node-field-aircraft-manufacturer.field-name-field-aircraft-manufacturer.field-type-entity-reference.field-label-above.has-single > div.field-items > div > a"
    manufacturer <- page |>
      html_elements(path) |>
      rvest::html_text2()

    path <- ".field-name-field-aircraft-type-code.field-type-entity-reference.field-label-above.has-single > div.field-items > div"
    type <- page |>
      html_elements(path) |>
      rvest::html_text2()

    path <- ".field-node-field-aircraft-wtc.field-name-field-aircraft-wtc.field-type-entity-reference.field-label-above.has-single > div.field-items > div"
    wtc <- page |>
      html_elements(path) |>
      rvest::html_text2() |>
      make_valid()

    recat_eu <- NA_character_

    path <- ".field-node-field-performance-to-mtow.field-name-field-performance-to-mtow.field-type-integer.field-label-above.has-single > div.field-items > div"
    mtow_kg <- page |>
      html_elements(path) |>
      rvest::html_text2() |>
      make_valid()

    path <- ".field-node-field-performance-cruise-tas.field-name-field-performance-cruise-tas.field-type-integer.field-label-above.has-single > div.field-items > div"
    cruise_tas_kt <- page |>
      html_elements(path) |>
      rvest::html_text2() |>
      make_valid()

    path <- ".field-name-field-performance-cruise-mach.field-type-string.field-label-above.has-single > div.field-items > div"
    cruise_mach <- page |>
      html_elements(path) |>
      rvest::html_text2() |>
      make_valid()

    path <- ".field-node-field-performance-cruise-range.field-name-field-performance-cruise-range.field-type-integer.field-label-above.has-single > div.field-items > div"
    cruise_range_nm <- page |>
      html_elements(path) |>
      rvest::html_text2() |>
      make_valid()

    path <- ".field-name-field-performance-cruise-ceiling.field-type-integer.field-label-above.has-single > div.field-items > div"
    cruise_ceiling_fl <- page |>
      html_elements(path) |>
      rvest::html_text2() |>
      make_valid()

    path <- ".field-node-field-aircraft-wing-span.field-name-field-aircraft-wing-span.field-type-float.field-label-above.has-single > div.field-items > div"
    wing_span_m <- page |>
      html_elements(path) |>
      rvest::html_text2() |>
      str_remove(" m$") |>
      as.numeric()

    path <- ".field-node-field-aircraft-length_m.field-name-field-aircraft-length.field-type-float.field-label-above.has-single > div.field-items > div"
    wing_span_m <- page |>
      html_elements(path) |>
      rvest::html_text2() |>
      str_remove(" m$") |>
      as.numeric()

    path <- ".field-node-field-aircraft-height.field-name-field-aircraft-height.field-type-float.field-label-above.has-single > div.field-items > div"
    height_m <- page |>
      html_elements(path) |>
      rvest::html_text2() |>
      str_remove(" m$") |>
      as.numeric()

    accomodation <- NA_character_

    # fmt: skip
    response <- tibble_row(
      icao           = icao,
      name           = name,
      manufacturer   = manufacturer,
      type           = type,
      wtc            = wtc,
      recat_eu       = recat_eu,
      mtow_kg        = mtow_kg,
      cruise_tas_kt  = cruise_tas_kt,
      cruise_mach    = cruise_mach,
      cruise_range_nm   = cruise_range_nm,
      cruise_ceiling_fl = cruise_ceiling_fl,
      wing_span_m      = wing_span_m,
      wing_span_m         = wing_span_m,
      height_m         = height_m,
      accomodation   = accomodation
    )
  }
  cli::cli_process_done()
  response
}


# ---- EUROCONTROL Perfromance DB ----
# Create a polite session
host <- "https://contentzone.eurocontrol.int/"
session <- polite::bow(host, force = TRUE)
scraper <- purrr::partial(scrape_aircraft_type_info_perfdb, session = session)

# some aircraft types will be missing, i.e. helicopters
all_acts <- ac_types |>
  map(.f = scraper) |>
  bind_rows()

source("R/act_passengers.R")

all_acts <- all_acts |>
  left_join(act_all_pax) |>
  write_csv("data/all_acts.csv")


# ---- SkyBrary aircraft pages ----
# Create a polite session
host <- "https://skybrary.aero/"
session <- polite::bow(host, force = TRUE)
scraper <- purrr::partial(scrape_aircraft_type_info_skybrary, session = session)


# scrape only missing type from perf DB
sky_acts <- all_acts |>
  filter(if_all(c(-icao), is.na)) |>
  pull(icao) |>
  map(.f = scraper) |>
  bind_rows()

sky_acts |>
  write_csv("sky_acts.csv")
