# Process Weekly Cholera Data from JHU: Convert to MOSAIC Format

This function processes raw weekly cholera surveillance data from the
JHU Public Surveillance Dataset, aggregates to country level, parses
ISO-weeks, computes week start/end dates, and prepares the data for the
MOSAIC framework.

## Usage

``` r
process_JHU_weekly_data(PATHS)
```

## Arguments

- PATHS:

  A list containing paths to the raw and processed data directories.
  Should include:

  - **DATA_RAW**: Path to the directory containing
    `Public_surveillance_dataset.rds`.

  - **DATA_JHU_WEEKLY**: Path to save the processed cholera data (e.g.,
    `cholera_country_weekly_processed.csv`).

## Value

Invisibly returns `NULL`. Side effects:

- Reads raw data with
  [`readRDS()`](https://rdrr.io/r/base/readRDS.html).

- Writes processed data to
  `PATHS$DATA_JHU_WEEKLY/cholera_country_weekly_processed.csv` via
  [`write.csv()`](https://rdrr.io/r/utils/write.table.html).

## Details

Steps performed:

- Load `Public_surveillance_dataset.rds` and keep only
  `spatial_scale == "country"`.

- Extract `iso_code` from `location_name` (suffix after “::”) and
  convert to full country name.

- Split `epiweek` (e.g. "2017-15") into `year` and `week`.

- Create `date_start` (Monday) and `date_stop` (Sunday) for each ISO
  week via
  [`ISOweek::ISOweek2date()`](https://rdrr.io/pkg/ISOweek/man/ISOweek2date.html).

- Compute `month` from `date_start` using
  [`lubridate::month()`](https://lubridate.tidyverse.org/reference/month.html).

- Define `cases` using suspected (`sCh`) if present, else confirmed
  (`cCh`), defaulting to 0.

- Define `deaths` (zero if `NA`).

- Save a data.frame with columns:
  `country, iso_code, year, month, week, date_start, date_stop, cases, deaths`.

## Examples

``` r
if (FALSE) { # \dontrun{
PATHS <- list(
  DATA_RAW        = "data/raw",
  DATA_JHU_WEEKLY = "data/processed/jhu_weekly"
)
process_JHU_weekly_data(PATHS)
} # }
```
