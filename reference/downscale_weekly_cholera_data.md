# Downscale Weekly Cholera Data to Daily Frequency

This function reads a CSV file containing weekly processed cholera data
from WHO and JHU sources and downscales it to a daily time series using
the
[`MOSAIC::downscale_weekly_values`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/downscale_weekly_values.md)
method. For each country (identified by its ISO code), the function
applies the `downscale_weekly_values` function to both the cases and
deaths columns, then merges the results by date.

## Usage

``` r
downscale_weekly_cholera_data(PATHS)
```

## Arguments

- PATHS:

  A list of file paths. This list must include an element named
  `DATA_WHO_WEEKLY` and `DATA_WHO_DAILY`.

## Value

Invisibly returns `NULL`.

## Details

The input CSV file `cholera_country_weekly_processed.csv` is expected to
contain at least the following columns:

- `date_start` - The start date of the week (in "YYYY-MM-DD" format,
  must be a Monday).

- `iso_code` - The ISO code identifying the country.

- `cases` - The weekly total number of cholera cases.

- `deaths` - The weekly total number of cholera deaths.

The function downsamples these weekly totals to daily counts using the
helper function `downscale_weekly_values`, which distributes any
remainders symmetrically across the days of each week when
`integer = TRUE`. The
[`MOSAIC::convert_iso_to_country`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/convert_iso_to_country.md)
function is used to convert the ISO code to a country name.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Define paths list with the directory containing the weekly data file:
  PATHS <- list(DATA_WHO_WEEKLY = "/path/to/your/data")

  # Downscale the weekly cholera data to daily data:
  downscale_weekly_cholera_data(PATHS)
} # }
```
