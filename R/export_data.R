# point profile for 1st Aug

library(conflicted)
library(tidyverse)
library(eurocontrol)
library(arrow)

withr::local_envvar(c(TZ = "UTC", ORA_SDTZ = "UTC", NLS_LANG = ".AL32UTF8"))
conn <- withr::local_db_connection(db_connection("PRU_DEV"))

bb <- c(xmin = -25.488281,
        xmax = 45.407181,
        ymin = 26.638253,
        ymax = 71.864780)


# Aug 2024
wef <- "2024-08-01" |> as_date()
til <- wef + ddays(1)

trjs <- point_profiles_tidy(conn = conn,
                    wef = wef,
                    til = til,
                    # bbox = bb,
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
                            # bbox = bb,
                            profile = "CTFM")

trjs |>
  collect() |>
  write_parquet(str_glue("data-raw/trjs_{dddd}.parquet", dddd = format(wef, "%Y-%m-%d")))
