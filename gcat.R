library(conflicted)
library(tidyverse)
library(here)
library(fs)

conflicts_prefer(dplyr::filter)

read_catalogue <- function(path) {
  fn <- fs::path_file(path) |> fs::path_ext_remove()
  gcat_file <- path
  gcat_date <- read_lines(gcat_file, skip = 1, n_max = 1) |>
    str_sub(start = 10L) |>
    as_datetime(format = "%Y %b  %e  %H%M:%S")

  gcat_col_names <- names(read_tsv(gcat_file, n_max = 0))
  gcat <- read_tsv(
    gcat_file,
    col_names = gcat_col_names,
    skip = 2,
    trim_ws = FALSE,
    quote = ""
  )
  gcat |>
    mutate(
      catalogue = fn,
      update_date = gcat_date
    )
}

gcat <- dir_ls(here("data-raw"), glob = "*.tsv") |>
  as_tibble() |>
  rename(path = value) |>
  mutate(
    filename = path_file(path)
  ) |>
  filter(filename != "currentcat.tsv") |>
  pull(path) |>
  map(.f = read_catalogue) |>
  bind_rows() |>
  janitor::clean_names()


is_upper_stage <- function(type) {
  str_detect(type, "R1|R2|R3|R4|R5")
}

is_rocket_component <- function(type) {
  str_detect(type, "^C..[A|M].*$")
}

end_date <- '2024 Mar 01' |> as_datetime()
start_date <- end_date - ddays(10 * 365.25)

# d_date = phase end time
gcat_base <- gcat |>
  # a reentry type
  filter(status == "R") |>
  mutate(
    string_date = str_sub(d_date, 1L, 11L)
  ) |>
  mutate(
    uncertainty = str_sub(d_date, -1L, -1L) == "?",
    d_date_length = str_length(d_date),
    # uncertainty = if_else(uncertainty == TRUE,
    #                       case_when(
    #                         d_date_length ==
    #                       )),
    reentry_date = if_else(is_upper_stage(type), str_sub(d_date, 1L, 11L), NA),
    object = if_else(is_upper_stage(type), "upper stage", NA)
  ) |>
  mutate(
    reentry_date = if_else(
      is_rocket_component(type),
      str_sub(d_date, 1L, 11L),
      reentry_date
    ),
    object = if_else(is_rocket_component(type), "rocket component", object)
  ) |>
  filter(!is.na(reentry_date)) |>
  mutate(
    reentry_date = as_datetime(reentry_date)
  )

gcat_base |>
  filter(uncertainty == TRUE) |>
  select(d_date, d_date_length, uncertainty) |>
  mutate(d_date_length = d_date_length - 1) |>
  pull(d_date_length) |>
  unique() |>
  sort()

gcat_base |>
  filter(start_date <= reentry_date, reentry_date < end_date) |>
  group_by(object) |>
  count()

# type = line[3] in python
# status = line[11] in python
