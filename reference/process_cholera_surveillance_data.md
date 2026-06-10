# Process Combined Weekly and Daily Cholera Surveillance Data with Truly Square Data Structure

This function reads the processed weekly cholera data from WHO, JHU, and
supplemental (SUPP) sources, labels each record by its source, combines
them (removing duplicate country-week entries according to the specified
source preference), creates a truly square data structure with all
country-week combinations from min to max date across the entire
dataset, and downscales the combined weekly totals to a daily time
series using
[`MOSAIC::downscale_weekly_values`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/downscale_weekly_values.md)
with integer allocation.

## Usage

``` r
process_cholera_surveillance_data(PATHS, include_ai = FALSE)
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

- include_ai:

  Logical (default `FALSE`). When `TRUE`, reads the AI-mined processed
  file (`DATA_AI_WEEKLY/cholera_country_weekly_processed.csv`, produced
  by
  [`process_AI_cholera_data`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/process_AI_cholera_data.md))
  as a fourth source, using its per-row `confidence_weight` and
  `disaggregation_method`. When `FALSE`, only WHO/JHU/SUPP are merged.
  If `include_ai = TRUE` but the AI file is missing/empty, a warning is
  emitted and the merge proceeds with the three direct sources.

  The output always carries a `confidence_weight` and a
  `disaggregation_method` column (stable schema regardless of this
  flag): direct-source observations (WHO/JHU/SUPP) get
  `confidence_weight = 1.0` (fully trusted) and
  `disaggregation_method = NA`; AI rows keep their per-row values; and
  square-grid cells with no observation get `confidence_weight = NA`.

## Value

Invisibly returns `NULL`. Side effects:

- Reads weekly CSVs from all three sources and adds a `source` column.

- Cleans rows with missing key grouping fields (iso_code, year, week).

- Harmonizes columns by taking the union across all sources, NA-filling
  any column missing from a given source (so source-specific columns are
  never silently dropped).

- Deduplicates by `iso_code`, `year`, `week` using the fixed priority
  WHO \> JHU \> AI \> SUPP (completeness tie-break).

- Creates truly square data structure with all country-week combinations
  from min to max date (missing data = NA).

- Saves the combined weekly data to
  `PATHS$DATA_CHOLERA_WEEKLY/cholera_surveillance_weekly_combined.csv`.

- Downscales weekly `cases` and `deaths` to daily counts, preserving
  square structure (keeping days with NA), and carries over the
  `source`.

- Saves the combined daily data to
  `PATHS$DATA_CHOLERA_DAILY/cholera_surveillance_daily_combined.csv`.

## Details

Duplicate country-week entries across sources are resolved by a fixed
priority **WHO \> JHU \> AI \> SUPP**, with a completeness tie-break:
within a key, rows with complete cases AND deaths are preferred before
the source priority is applied.
