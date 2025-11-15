# Download, Process, and Save Historical Annual WHO Cholera Data for Africa

This function compiles historical cholera data from the WHO for African
countries (AFRO region) for the years 1949 to 2024. The data is
processed and saved in the MOSAIC directory structure. It includes
reported cholera cases, deaths, and case fatality ratios (CFR).

## Usage

``` r
process_WHO_annual_data(PATHS)
```

## Arguments

- PATHS:

  A list containing the paths to raw and processed data directories.
  Typically generated from
  [`get_paths()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_paths.md)
  and should include:

  - **DATA_RAW**: Path to the directory containing raw WHO cholera data.

  - **DATA_WHO_ANNUAL**: Path to the directory where processed cholera
    data will be saved.

## Value

The function does not return a value but processes and saves the
historical WHO cholera data into CSV files in the processed data
directory.

## Details

This function compiles cholera data for African countries (AFRO region)
by processing historical data from 1949-2024. The data sources are as
follows:

- **1949-2021**: Data were compiled from WHO annual reports by **Our
  World In Data**. See <https://ourworldindata.org>.

- **2022**: Data were manually extracted from the WHO annual report.

- **2023-2024**: Data are downloaded directly from the **WHO AWD GIS
  dashboard**.

Additionally, the function calculates 95% binomial confidence intervals
for the CFR where applicable, and the data is saved as CSV files in the
processed data directory.

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming PATHS is generated from get_paths()
PATHS <- get_paths()

# Download and process WHO annual cholera data
process_WHO_annual_data(PATHS)
} # }
```
