# Process UN World Prospects Data for African Countries

This function processes demographic data from the UN World Population
Prospects for African countries. It loads available data, gets total
population, birth rates, and death rates, and saves the processed data
as both annual and daily CSV files. The daily data is created through
linear interpolation.

## Usage

``` r
process_UN_demographics_data(PATHS)
```

## Arguments

- PATHS:

  A list containing paths where raw and processed data are stored. PATHS
  is typically the output of the
  [`get_paths()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_paths.md)
  function and should include the following components:

  - **DATA_RAW**: Path to the directory containing the raw UN World
    Population Prospects data.

  - **DATA_PROCESSED**: Path to the directory where processed
    demographic data should be saved.

## Value

The function does not return a value. It processes the UN demographic
data and saves both annual and daily interpolated results as CSV files
in the specified directory.

## Details

The function performs the following steps:

1.  Loads raw demographic data from CSV files.

2.  Merges population, birth rate, and death rate data.

3.  Saves the annual data as `UN_world_population_prospects_annual.csv`.

4.  Creates daily interpolated data using linear interpolation between
    annual values.

5.  Saves the daily data as `UN_world_population_prospects_daily.csv`.

The processed data files will be saved in the
`PATHS$DATA_PROCESSED/demographics/` directory.

Data Source: [United Nations World Population
Prospects](https://population.un.org/wpp/)

## Examples

``` r
if (FALSE) { # \dontrun{
# Define paths for raw and processed data using get_paths()
PATHS <- get_paths()

# Process the UN World Prospects data for all available years and save the results
process_demographics_data(PATHS)
} # }
```
