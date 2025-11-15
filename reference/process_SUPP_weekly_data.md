# Process supplementary cholera outbreak data from raw SUPP directory

This function reads the first CSV line-list file in the raw SUPP
directory (e.g., Aceng et al. 2025 S1 Data), cleans and standardizes the
data, and returns a data.frame of weekly counts in the standard MOSAIC
surveillance format.

## Usage

``` r
process_SUPP_weekly_data(PATHS)
```

## Arguments

- PATHS:

  A list of file paths used by the MOSAIC pipeline. Must include:

  DATA_RAW

  :   Root data directory (e.g., root/MOSAIC-data/raw).

## Value

A data.frame with columns:

- country:

  Country name (fixed to "Uganda").

- iso_code:

  ISO3 country code (fixed to "UGA").

- year:

  Calendar year of week.

- month:

  Calendar month (1–12) of week start.

- week:

  Epidemiological week number (1–52).

- date_start:

  Start date of week (Monday).

- date_stop:

  End date of week (Sunday).

- cases:

  Number of onset dates in that week.

- deaths:

  Number of outcomes coded as death in that week.

- cases_binary:

  Indicator (0/1) if any cases \>0.
