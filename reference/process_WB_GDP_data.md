# Process World Bank GDP data

Reads the raw World Bank GDP CSV, reshapes it to long format via a for
loop, writes the processed data as a CSV to disk, and saves it to
processed data.

## Usage

``` r
process_WB_GDP_data(PATHS)
```

## Arguments

- PATHS:

  List. Project paths object with components `DATA_RAW` and
  `DATA_PROCESSED`.

  - Expects the raw file at:
    `file.path(PATHS$DATA_RAW, 'world_bank', 'API_NY.GDP.MKTP.CD_DS2_en_csv_v2_132025.csv')`

  - Will write output to:
    `file.path(PATHS$DATA_PROCESSED, 'world_bank', 'GDP_data_world_bank.csv')`

## Value

A data.frame with columns:

- iso_code:

  ISO country code (character)

- year:

  Year (integer)

- GDP:

  Gross Domestic Product (numeric)

Invisibly returns the data.frame after writing the CSV.
