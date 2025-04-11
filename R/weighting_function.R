# ---- weighting function ----
# build reentry probability density from past debris reentry episodes
library(conflicted)
library(tidyverse)
library(magrittr)

conflicts_prefer(dplyr::filter)
rE <- 6378e3 # Earth equatorial radius
alt <- 550e3 # altitude of satellite, i.e. 500 km for LEO

source(here::here("R", "orbitals.R"))

gcat <- read_csv("data/gcat.csv") |>
  # not strictly needed, because there are unique rows
  filter(row_number() == 1, .by = c("jcat", "piece")) |>
  select(jcat, piece, inclination, reentry_date) |>
  mutate(
    id = str_c(jcat, "_", piece)
  )

fff <- purrr::partial(
  latitude_weights,
  delta_lat = 0.5,
  altitude = rE + alt
)

gcat_orig <- read_csv(
  "../AirspaceDebris/Output data/10_year_reentries.csv",
  col_names = FALSE
) |>
  select(jcat = X1, piece = X3, inclination = X37, reentry_date = X11) |>
  mutate(
    reentry_date = str_sub(reentry_date, start = 1L, end = 12L) |> as_date(),
    # normalize latitude to [-90, 90] range
    inclination = ((inclination + 90) %% 180) - 90,
    id = str_c(jcat, "_", piece)
  )


uuu <- gcat |>
  # filter(id %in% (gcat_orig |> pull(id))) |>
  pull(inclination) |>
  purrr::map(
    \(.x) fff(inclination_deg = .x)
    # |>
    #   pivot_wider(names_from = lat, values_from = val)
  ) |>
  bind_cols() |>
  rename(lll = `lat...1`) |>
  select(-starts_with("lat")) |>
  mutate(
    vvv = rowSums(pick(starts_with("val"))),
    nnn = length(pick(starts_with("val"))),
    vvv = vvv / nnn
  ) |>
  select(-starts_with("val"), -nnn) |>
  rename(lat = "lll", val = "vvv")

uuu |>
  ggplot() +
  geom_line(aes(x = lat, y = val), linewidth = 1.3) +
  scale_x_continuous("latitude", breaks = c(-90, -60, -30, 0, 30, 60, 90)) +
  theme_minimal(base_size = 24) +
  theme(
    axis.title.y = element_blank(),
    panel.grid.minor.x = element_blank(),
  )

ggsave("media/figures/weighting.png")
