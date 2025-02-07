# read raw trajectories and prepare for resample by the traffic library
library(duckdb)
library(dplyr, warn.conflicts = FALSE)

withr::local_envvar(c(TZ = "UTC", ORA_SDTZ = "UTC"))
con <- withr::local_db_connection(DBI::dbConnect(duckdb(), path = ":memory:"))

dbExecute(con, "SET extension_directory = '/Users/spi/.duckdb/extensions';")
# dbExecute(con, "INSTALL H3;")
dbExecute(con, "LOAD H3;")
# dbExecute(con, "INSTALL SPATIAL;")
# dbExecute(con, "LOAD SPATIAL;")

dbExecute(con, "
  CREATE OR REPLACE TABLE TRAJECTORY AS
    SELECT
      FLIGHT_ID AS flight_id,
      ICAO24 AS icao24,
      CALLSIGN AS callsign,
      TIME_OVER AS timestamp,
      LONGITUDE AS longitude,
      LATITUDE  AS latitude,
      FLIGHT_LEVEL * 100 AS altitude,
      SEQ_ID AS sequence_id,
      h3_h3_to_string(h3_latlng_to_cell(latitude, longitude, 2)) AS cell
    FROM
      'data-raw/trjs_2024-08-01.parquet'
    WHERE
      -- filter on bounding box (the one containing all HEX cells containing NM area)
      -- xmin = -27.78166, ymin = 24.90098,
      -- xmax =  48.56037, ymax = 72.85065
      (
        (-27.78166 <= longitude AND longitude < 48.56037)
          AND (24.90098 <= latitude AND latitude < 72.85065)
      )
    ;")


dbExecute(con, "
  COPY
      (SELECT * FROM TRAJECTORY)
      TO 'data/trajectory_2024-08-01_pre.parquet'
      (FORMAT 'parquet');
          ")




dbExecute(con, "
  CREATE OR REPLACE TABLE TRAJECTORY AS
    SELECT
      FLIGHT_ID AS flight_id,
      ICAO24 AS icao24,
      CALLSIGN AS callsign,
      TIME_OVER AS timestamp,
      LONGITUDE AS longitude,
      LATITUDE  AS latitude,
      FLIGHT_LEVEL * 100 AS altitude,
      SEQ_ID AS sequence_id,
      h3_h3_to_string(h3_latlng_to_cell(latitude, longitude, 2)) AS cell
    FROM
      'data-raw/trjs_2024-12-05.parquet'
    WHERE
      -- filter on bounding box (the one containing all HEX cells containing NM area)
      -- xmin = -27.78166, ymin = 24.90098,
      -- xmax =  48.56037, ymax = 72.85065
      (
        (-27.78166 <= longitude AND longitude < 48.56037)
          AND (24.90098 <= latitude AND latitude < 72.85065)
      )
    ;")


dbExecute(con, "
  COPY
      (SELECT * FROM TRAJECTORY)
      TO 'data/trajectory_2024-12-05_pre.parquet'
      (FORMAT 'parquet');
          ")
