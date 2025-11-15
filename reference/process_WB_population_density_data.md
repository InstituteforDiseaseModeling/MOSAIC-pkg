# Process World Bank Population Density Data

Reads the raw World Bank population density CSV (skipping the first 4
metadata rows), reshapes it to long format via a for loop, writes the
processed data as a CSV to disk, and returns it. Uses base R functions
only.

## Usage

``` r
process_WB_population_density_data(PATHS)
```

## Arguments

- PATHS:

  List. Project paths object with components `DATA_RAW` and
  `DATA_PROCESSED`.

  - Expects the raw file at:
    `file.path(PATHS$DATA_RAW, 'world_bank', 'API_EN.POP.DNST_DS2_en_csv_v2_85433.csv')`

  - Will write output to:
    `file.path(PATHS$DATA_PROCESSED, 'world_bank', 'pop_density_data_world_bank.csv')`

## Value

A data.frame with columns:

- iso_code:

  ISO country code (character)

- year:

  Year (integer)

- pop_density:

  Population density (numeric, people per sq. km)

Invisibly returns the data.frame after writing the CSV.
