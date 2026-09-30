# Process World Bank Urban Population Proportion Data

Reads the raw World Bank urban population proportion CSV, reshapes it to
long format via a for loop, writes the processed data as a CSV to disk,
and saves it to processed data.

## Usage

``` r
process_WB_urban_population_data(PATHS)
```

## Arguments

- PATHS:

  List. Project paths object with components `DATA_RAW` and
  `DATA_PROCESSED`.

  - Reads the newest raw CSV for indicator `SP.URB.TOTL.IN.ZS` in
    `file.path(PATHS$DATA_RAW, 'world_bank', 'urban_population')` (see
    [`download_WB_data()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/download_WB_data.md)).

  - Writes
    `file.path(PATHS$DATA_PROCESSED, 'world_bank', 'world_bank_urban_population_data.csv')`,
    creating the directory if needed.

## Value

A data.frame with columns:

- iso_code:

  ISO country code (character)

- year:

  Year (integer)

- urban_pop_prop:

  Urban population proportion (numeric, percent of total population)

Invisibly returns the data.frame after writing the CSV.
