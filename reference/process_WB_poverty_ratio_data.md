# Process World Bank Poverty Ratio Data

Reads the raw World Bank poverty ratio CSV (skipping the first 4
metadata rows), reshapes it to long format via a for loop, writes the
processed data as a CSV to disk, and returns it. Uses base R functions
only.

## Usage

``` r
process_WB_poverty_ratio_data(PATHS)
```

## Arguments

- PATHS:

  List. Project paths object with components `DATA_RAW` and
  `DATA_PROCESSED`.

  - Expects the raw file located in:
    `file.path(PATHS$DATA_RAW, 'world_bank', '<poverty_ratio_filename>.csv')`

  - Will write output to:
    `file.path(PATHS$DATA_PROCESSED, 'world_bank', 'poverty_ratio_data_world_bank.csv')`

## Value

A data.frame with columns:

- iso_code:

  ISO country code (character)

- year:

  Year (integer)

- poverty_ratio:

  Poverty ratio (numeric, percent of population below poverty line)

Invisibly returns the data.frame after writing the CSV.
