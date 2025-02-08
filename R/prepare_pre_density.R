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
    *,
    ROW_NUMBER() OVER(PARTITION BY flight_id ORDER BY timestamp) AS seq_id,
    date_part('day',   timestamp) AS day,
    date_part('month', timestamp) AS month,
    date_part('year',  timestamp) AS year,
    date_part('hour',  timestamp) AS hour,
    h3_h3_to_string(h3_latlng_to_cell(latitude, longitude, 2)) AS cell,
    2 AS h3_resolution
  FROM
      'data/trajectories_2024-08-01_resampled_30s.parquet';
  ALTER TABLE TRAJECTORY DROP COLUMN sequence_id;
  ALTER TABLE TRAJECTORY RENAME seq_id TO sequence_id;
  COPY(
    SELECT
      *
    FROM
      TRAJECTORY
    WHERE
      -- filter on bounding box (the one containing all HEX cells containing NM area)
      -- xmin = -27.78166, ymin = 24.90098,
      -- xmax =  48.56037, ymax = 72.85065
      (
        (-27.78166 <= longitude AND longitude < 48.56037)
          AND (24.90098 <= latitude AND latitude < 72.85065)
      )
      AND (year = 2024 AND month = 8 AND day = 1)
  )
  TO
    'data/trajectories_2024-08-01_resampled_30s_bbox_res_2.parquet'
  (FORMAT 'parquet')
  ;")

# res 3
dbExecute(con, "
  CREATE OR REPLACE TABLE TRAJECTORY AS
  SELECT
    *,
    ROW_NUMBER() OVER(PARTITION BY flight_id ORDER BY timestamp) AS seq_id,
    date_part('day',   timestamp) AS day,
    date_part('month', timestamp) AS month,
    date_part('year',  timestamp) AS year,
    date_part('hour',  timestamp) AS hour,
    h3_h3_to_string(h3_latlng_to_cell(latitude, longitude, 3)) AS cell,
    3 AS h3_resolution
  FROM
      'data/trajectories_2024-08-01_resampled_30s.parquet';
  ALTER TABLE TRAJECTORY DROP COLUMN sequence_id;
  ALTER TABLE TRAJECTORY RENAME seq_id TO sequence_id;
  COPY(
    SELECT
      *
    FROM
      TRAJECTORY
    WHERE
      -- filter on bounding box (the one containing all HEX cells containing NM area)
      -- xmin = -27.78166, ymin = 24.90098,
      -- xmax =  48.56037, ymax = 72.85065
      (
        (-27.78166 <= longitude AND longitude < 48.56037)
          AND (24.90098 <= latitude AND latitude < 72.85065)
      )
      AND (year = 2024 AND month = 8 AND day = 1)
  )
  TO
    'data/trajectories_2024-08-01_resampled_30s_bbox_res_3.parquet'
  (FORMAT 'parquet')
  ;")








dbExecute(con, "
  CREATE OR REPLACE TABLE TRAJECTORY AS
  SELECT
    *,
    ROW_NUMBER() OVER(PARTITION BY flight_id ORDER BY timestamp) AS seq_id,
    date_part('day',   timestamp) AS day,
    date_part('month', timestamp) AS month,
    date_part('year',  timestamp) AS year,
    date_part('hour',  timestamp) AS hour,
    h3_h3_to_string(h3_latlng_to_cell(latitude, longitude, 2)) AS cell,
    2 AS h3_resolution
  FROM
      'data/trajectories_2024-12-05_resampled_30s.parquet'
  ;
  ALTER TABLE TRAJECTORY DROP COLUMN sequence_id;
  ALTER TABLE TRAJECTORY RENAME seq_id TO sequence_id;
  COPY(
    SELECT
      *
    FROM
      TRAJECTORY
    WHERE
      -- filter on bounding box (the one containing all HEX cells containing NM area)
      -- xmin = -27.78166, ymin = 24.90098,
      -- xmax =  48.56037, ymax = 72.85065
      (
        (-27.78166 <= longitude AND longitude < 48.56037)
          AND (24.90098 <= latitude AND latitude < 72.85065)
      )
      AND (year = 2024 AND month = 12 AND day = 5)
  )
  TO
    'data/trajectories_2024-12-05_resampled_30s_bbox_res_2.parquet'
  (FORMAT 'parquet')
  ;")


# res 3
dbExecute(con, "
  CREATE OR REPLACE TABLE TRAJECTORY AS
  SELECT
    *,
    ROW_NUMBER() OVER(PARTITION BY flight_id ORDER BY timestamp) AS seq_id,
    date_part('day',   timestamp) AS day,
    date_part('month', timestamp) AS month,
    date_part('year',  timestamp) AS year,
    date_part('hour',  timestamp) AS hour,
    h3_h3_to_string(h3_latlng_to_cell(latitude, longitude, 3)) AS cell,
    3 AS h3_resolution
  FROM
      'data/trajectories_2024-12-05_resampled_30s.parquet'
  ;
  ALTER TABLE TRAJECTORY DROP COLUMN sequence_id;
  ALTER TABLE TRAJECTORY RENAME seq_id TO sequence_id;
  COPY(
    SELECT
      *
    FROM
      TRAJECTORY
    WHERE
      -- filter on bounding box (the one containing all HEX cells containing NM area)
      -- xmin = -27.78166, ymin = 24.90098,
      -- xmax =  48.56037, ymax = 72.85065
      (
        (-27.78166 <= longitude AND longitude < 48.56037)
          AND (24.90098 <= latitude AND latitude < 72.85065)
      )
      AND (year = 2024 AND month = 12 AND day = 5)
  )
  TO
    'data/trajectories_2024-12-05_resampled_30s_bbox_res_3.parquet'
  (FORMAT 'parquet')
  ;")
