# Download, Process, and Save Historical Annual WHO Cholera Data for Africa

Compiles cholera surveillance data from WHO for African countries (AFRO
region) from 1949 onward. Data sources and the precise per-year coverage
are tracked through the pipeline so partial-year snapshots (current year
from the WHO Global Cholera & AWD dashboard) are usable downstream
without being mislabeled as full-year totals.

## Usage

``` r
process_WHO_annual_data(PATHS)
```

## Arguments

- PATHS:

  A list containing the paths to raw and processed data directories.
  Typically generated from
  [`get_paths()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_paths.md).

## Value

The function does not return a value but writes processed CSVs to
`PATHS$DATA_WHO_ANNUAL`.

## Details

Data sources:

- **1949-2021**: WHO annual reports compiled by Our World in Data. See
  <https://ourworldindata.org>.

- **2022**: Manually transcribed from the WHO 2022 annual cholera report
  (PDF).

- **2023+**: Downloaded from the **WHO Global Cholera & AWD dashboard**
  via ArcGIS. Each download is archived to
  `raw/WHO/annual/who_global_dashboard/cholera_adm0_public_snapshot_YYYY-MM-DD.csv`
  and ALL files matching `cholera_adm0_public_*.csv` in the dashboard
  directory are ingested. Each row's actual coverage is derived from
  `first_epiwk` / `last_epiwk` (rolling snapshots cross calendar-year
  boundaries – see the year-labeling note below).

**Year labeling**: The ArcGIS dashboard returns one row per country
summing `case_total` / `death_total` between `first_epiwk` and
`last_epiwk`. Each row is assigned to the calendar year that contains
the majority of its days. `coverage_days` reports how many calendar days
are covered, and `year_fraction = coverage_days/365.25` indicates
whether the row is a full-year observation (~1.0) or a partial snapshot
(\<1.0). When multiple files have a row for the same `(iso_code, year)`
combination, the row with the larger `coverage_days` is kept.

**To refresh through a given calendar year**: place a year-filtered CSV
in `raw/WHO/annual/who_global_dashboard/` named
`cholera_adm0_public_YYYY.csv` (downloaded from the WHO dashboard UI
with the year filter applied). Re-run `process_WHO_annual_data()`.

Outputs (in `PATHS$DATA_WHO_ANNUAL`):

- `who_afro_annual.csv` – current full series, columns
  `country, iso_code, region, year, cases_total, cases_imported, deaths_total, cfr, cfr_lo, cfr_hi, first_epiwk, last_epiwk, coverage_days, year_fraction, source`.

- Intermediate per-source slices for traceability.

## Examples

``` r
if (FALSE) { # \dontrun{
PATHS <- get_paths()
process_WHO_annual_data(PATHS)
} # }
```
