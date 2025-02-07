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

withr::local_envvar(c(TZ = "UTC", ORA_SDTZ = "UTC", NLS_LANG = ".AL32UTF8"))
conn <- withr::local_db_connection(db_connection("PRU_DEV"))



# bbox Europe/EUROCONTROL area
# -25.488281,26.638253,45.407181,71.864780
bb_nm <- c(
  xmin = -25.488281,
  ymin =  26.638253,
  xmax =  45.407181,
  ymax =  71.864780) |>
  st_bbox(crs = 4326) |>
  st_as_sfc(crs = 4326) |>
  # add points to the bbox polygon
  densify(n = 300L) |>
  st_as_sf(res = 2)



res <- 2
cells <- bb_nm |>
  st_transform(crs = 4326) |>
  polygon_to_cells(res = res, simple = FALSE)

eur_hex <- cells |>
  pull(h3_addresses) |>
  unlist() |>
  cell_to_polygon(simple = FALSE)

# take the bbox of the union of hexes at resolution 2 as the bbox for the query
eur_hex_union <- eur_hex |>
  st_union() |>
  st_exterior_ring() |>
  st_as_sf() |>
  mutate(resolution = res)

bb <- eur_hex |>
  st_bbox()


# Aug 2024
wef <- "2024-08-01" |> as_date()
til <- wef + ddays(1)

trjs <- point_profiles_tidy(conn = conn,
                    wef = wef,
                    til = til,
                    bbox = bb,
                    profile = "CTFM")

trjs |>
  collect() |>
  write_parquet(str_glue("data-raw/trjs_{dddd}.parquet", dddd = format(wef, "%Y-%m-%d")))


# Dec 2024
wef <- "2024-12-05" |> as_date()
til <- wef + ddays(1)

trjs <- point_profiles_tidy(conn = conn,
                            wef = wef,
                            til = til,
                            bbox = bb,
                            profile = "CTFM")

trjs |>
  collect() |>
  write_parquet(str_glue("data-raw/trjs_{dddd}.parquet", dddd = format(wef, "%Y-%m-%d")))
