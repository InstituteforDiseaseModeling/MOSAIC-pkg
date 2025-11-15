# Process and Save OAG Flight Data for Africa

This function processes raw OAG flight data for African countries,
including data cleaning, location and date conversion, and aggregation.
The processed data is saved as CSV files for all African countries and a
subset of countries used in the MOSAIC modeling framework.

## Usage

``` r
process_OAG_data(PATHS)
```

## Arguments

- PATHS:

  A list containing the paths where raw and processed data are stored.
  PATHS is typically the output of the
  [`get_paths()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_paths.md)
  function and should include the following components:

  - **DATA_RAW**: Path to the directory containing the raw data files.

  - **DATA_PROCESSED**: Path to the directory where processed data
    should be saved.

  - **DATA_SHAPEFILES**: Path to the directory containing shapefiles for
    African countries.

## Value

The function processes the OAG flight data and saves the aggregated
results to the specified processed data paths.

## Examples

``` r
if (FALSE) { # \dontrun{
# Define paths for raw and processed data using get_paths()
PATHS <- get_paths()

# Process the OAG data and save the results
process_OAG_data(PATHS)
} # }
```
