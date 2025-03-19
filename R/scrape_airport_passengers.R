# To clean data
library(tidyverse)
library(lubridate)
library(janitor)
library(here)

# To scrape data
library(rvest)
library(httr)
library(polite)
library(magrittr)


url <- "https://en.wikipedia.org/wiki/List_of_the_busiest_airports_in_Europe#2024"
url_bow <- polite::bow(url)


eu_pass <- url_bow |>
  polite::scrape() |>
  rvest::html_nodes("table.wikitable") |>
  rvest::html_table(header = TRUE, trim = TRUE, dec = ".") |>
  simplify() |>
  extract2(1) |>
  clean_names() |>
  slice(-1) |>
  select(-2) |>
  select(1:5) |>
  mutate(
    rank2024 = as.integer(rank2024),
    passengers = str_remove(passengers, "\\[\\d*\\]$"),
    passengers = str_remove_all(passengers, ","),
    passengers = as.numeric(passengers),
    NULL
  ) |>
  mutate(
    year = 2024,
    passengers = if_else(city_served == "Toulouse", 7844953, passengers),
    passengers = if_else(
      airport == "Brussels South Charleroi Airport",
      10500000,
      passengers
    ),
    # keep same number as 2023
    passengers = if_else(
      airport == "Bordeaux–Mérignac Airport",
      6584194,
      passengers
    ),
    passengers = if_else(
      airport == "Beauvais–Tillé Airport",
      5638955,
      passengers
    ),
    NULL
  ) |>
  rename(rank = rank2024)

eu_pass |>
  write_csv(here("data", "airport_passengers.csv"))
