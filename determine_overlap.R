library(tidyverse)
library(lubridate)

# Read your CSV from Google Sheets export
raw <- data_file |> janitor::clean_names()

dat <- raw |>
  mutate(
    # Directly use your existing columns
    data_source  = data_source,
    rec_hash     = duplicate_finder,   # your existing hash
    id_tracker   = id_tracker,
    site_id_col  = site_id,
    sample_id_col = sample_id,
    analyte_col  = analyte,
    result_col   = sample_concentration,
    units_col    = sample_unit,
    sample_dt    = parse_date_time(sample_date, orders = c("Ymd", "Y-m-d", "m/d/Y")),
    lat_col      = latitude,
    lon_col      = longitude
  ) |>
  filter(!is.na(data_source) & data_source != "")

# Count by data source
count_by_source <- dat |>
  distinct(data_source, rec_hash, id_tracker) |>
  count(data_source, name = "n_unique_records") |>
  arrange(desc(n_unique_records))
print(count_by_source)

# Overlaps between data sources
db_hashes <- dat |> distinct(data_source, rec_hash, id_tracker)
sources <- sort(unique(db_hashes$data_source))
pairs <- as_tibble(t(combn(sources, 2))) |> rename(a = V1, b = V2)

overlaps <- pairs |>
  rowwise() |>
  mutate(
    n_a = sum(db_hashes$data_source == a),
    n_b = sum(db_hashes$data_source == b),
    n_intersect = nrow(inner_join(
      filter(db_hashes, data_source == a),
      filter(db_hashes, data_source == b),
      by = "rec_hash"
    )),
    jaccard = n_intersect / (n_a + n_b - n_intersect),
    prop_a_in_b = n_intersect / n_a,
    prop_b_in_a = n_intersect / n_b
  ) |>
  ungroup() |>
  arrange(desc(jaccard))
print(overlaps)

overlaps |> kable() |> kable_styling()
