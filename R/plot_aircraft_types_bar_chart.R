library(aviodebris)
library(tidyverse)
library(arrow)
library(here)

day <- "2024-07-05" |> as_date()

flts <- here("data", "trajectories_2024-07-05_resampled_30s.parquet") |>
  read_parquet()

flts |>
  filter(day <= timestamp, timestamp < (day + 1)) |>
  distinct(flight_id) |>
  count()

flts |>
  filter(day <= timestamp, timestamp < (day + 1)) |>
  ## turn into lumped factors with capitalized names
  dplyr::mutate(
    aircraft_type = forcats::fct_lump(aircraft_type, n = 30)
  ) |>
  count(aircraft_type, sort = TRUE)


flts |>
  filter(day <= timestamp, timestamp < (day + 1)) |>
  count(aircraft_type, sort = TRUE) |>
  ## add percentage label with `scales::percent()`
  dplyr::mutate(
    p = n / sum(n),
    perc = scales::percent(p, accuracy = .1, trim = FALSE),
    perc = if_else(row_number() == 1, paste(perc, "of all types"), perc)
  ) |>
  filter(p >= 0.01) |>
  summarise(tr = sum(p))

ccc <- flts |>
  filter(day <= timestamp, timestamp < (day + 1)) |>
  ## turn into lumped factors with capitalized names
  dplyr::mutate(
    aircraft_type = forcats::fct_lump(aircraft_type, n = 19)
  ) |>
  count(aircraft_type, sort = TRUE) |>
  ## order factor levels by number, put "Other" to end
  dplyr::mutate(
    aircraft_type = forcats::fct_rev(forcats::fct_inorder(aircraft_type)),
    aircraft_type = forcats::fct_relevel(aircraft_type, "Other", after = 0)
  ) |>
  ## add percentage label with `scales::percent()`
  dplyr::mutate(
    p = n / sum(n),
    perc = scales::percent(p, accuracy = .1, trim = FALSE),
    perc = if_else(row_number() == 1, paste(perc, "of all types"), perc)
  ) |>
  mutate(
    ## set justification based on data
    ## so that only the first label is placed inside
    place = if_else(row_number() <= 10, 1, 0),
    ## add some spacing to labels since we cant use nudge_x anymore
    perc = paste(" ", perc, "")
  ) |>
  mutate(
    color = case_when(
      0.05 <= p ~ "goldenrod1",
      (0.01 <= p) & (p < 0.05) ~ "mediumpurple1",
      aircraft_type == "Other" ~ "gray85",
      ## all others should be gray
      TRUE ~ "gray70"
    )
  )

ccc |>
  ggplot(aes(x = n, y = aircraft_type, fill = color)) +
  geom_col(fill = "gray70") +
  ## add percentage labels
  geom_text(aes(label = perc, hjust = place), size = 4, fontface = "bold") +
  ## reduce spacing between labels and bars
  scale_x_continuous(expand = c(.005, .01)) +
  theme_void() +
  theme(
    axis.text.y = element_text(size = 14, hjust = 1),
    plot.margin = margin(rep(15, 4))
  )

ggsave("media/figures/aircraft_types.png")
