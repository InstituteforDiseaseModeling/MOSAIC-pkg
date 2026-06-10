# Process UNICEF Child Malnutrition Data

Reads the "Survey Estimates" sheet from the raw UNICEF malnutrition
Excel file, drops footnote columns, reshapes key indicators to long
format, and writes a CSV.

## Usage

``` r
process_UNICEF_malnutrition_data(PATHS)
```

## Arguments

- PATHS:

  A named list of file paths from
  [`get_paths`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_paths.md),
  providing `DATA_RAW` (root of the raw UNICEF Excel file) and
  `DATA_PROCESSED` (destination for the processed CSV).

## Value

Invisibly returns the processed data frame (long format).
