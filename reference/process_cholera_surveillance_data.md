# Process Combined Weekly and Daily Cholera Surveillance Data with Truly Square Data Structure

This function reads the processed weekly cholera data from WHO, JHU, and
supplemental (SUPP) sources, labels each record by its source, combines
them (removing duplicate country–week entries according to the specified
source preference), creates a truly square data structure with all
country-week combinations from min to max date across the entire
dataset, and downscales the combined weekly totals to a daily time
series using
[`MOSAIC::downscale_weekly_values`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/downscale_weekly_values.md)
with integer allocation.

## Usage

``` r
process_cholera_surveillance_data(PATHS, keep_source = c("WHO", "JHU", "SUPP"))
```

## Arguments

- PATHS:

  A list of file paths. Must include:

  - **DATA_WHO_WEEKLY**: Directory containing
    `cholera_country_weekly_processed.csv` from WHO.

  - **DATA_JHU_WEEKLY**: Directory containing
    `cholera_country_weekly_processed.csv` from JHU.

  - **DATA_SUPP_WEEKLY**: Directory containing
    `cholera_country_weekly_processed.csv` from supplemental source (may
    include extra columns).

  - **DATA_CHOLERA_WEEKLY**: Directory where the combined weekly output
    will be saved.

  - **DATA_CHOLERA_DAILY**: Directory where the combined daily output
    will be saved.

- keep_source:

  Character; one of "WHO", "JHU", or "SUPP". When duplicate country–week
  entries occur across sources, the entry from `keep_source` will be
  used preferentially.

## Value

Invisibly returns `NULL`. Side effects:

- Reads weekly CSVs from all three sources and adds a `source` column.

- Cleans rows with missing key grouping fields (iso_code, year, week).

- Harmonizes columns by keeping only those present in all three
  dataframes.

- Deduplicates by `iso_code`, `year`, `week`, choosing rows from
  `keep_source`.

- Creates truly square data structure with all country-week combinations
  from min to max date (missing data = NA).

- Saves the combined weekly data to
  `PATHS$DATA_CHOLERA_WEEKLY/cholera_surveillance_weekly_combined.csv`.

- Downscales weekly `cases` and `deaths` to daily counts, preserving
  square structure (keeping days with NA), and carries over the
  `source`.

- Saves the combined daily data to
  `PATHS$DATA_CHOLERA_DAILY/cholera_surveillance_daily_combined.csv`.
