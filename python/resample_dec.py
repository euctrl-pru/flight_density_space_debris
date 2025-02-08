from traffic.core import Traffic

(Traffic.from_file('data-raw/trjs_2024-12-05.parquet')
  .resample(rule = "30s")
  .eval()
  .to_parquet('data/trajectories_2024-12-05_resampled_30s.parquet',
              index = False)
)
