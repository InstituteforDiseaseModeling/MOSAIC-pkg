# Process UNICEF Child Malnutrition Data

Reads the "Survey Estimates" sheet from the raw UNICEF malnutrition
Excel file, drops footnote columns, reshapes key indicators to long
format, and writes a CSV.

## Usage

``` r
process_UNICEF_malnutrition_data(PATHS)
```

## Arguments

- paths:

  A named list of file paths with components:

  - raw\$malnutrition_data: path to the raw Excel file

  - processed\$malnutrition_data: path to write the processed CSV

## Value

Invisibly returns the processed data frame (long format).
