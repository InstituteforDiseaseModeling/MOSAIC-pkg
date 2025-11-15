# Estimate Daily Demographic Rates from UN World Population Prospects Data

This function estimates daily population, birth rates, and death rates
for countries in the MOSAIC framework based on the UN World Population
Prospects data. The function interpolates annual demographic data to
daily values and applies an optional smoothing method to generate
continuous time-series estimates. The function can produce smoothed
times series of any length from 1967 to 2100 (the extent of processed UN
data and model projections).

## Usage

``` r
est_demographic_rates(PATHS, date_start, date_stop, smooth_method)
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

  - **DATA_WHO_ANNUAL**: Path to the directory containing annual WHO
    cholera mortality data.

  - **MODEL_INPUT**: Path to the directory where model input parameter
    files should be saved.

- date_start:

  The starting date for demographic parameters. Accepts `Date` class
  objects or valid character strings.

- date_stop:

  The ending date for data demographic parameters. Must be in a valid
  date format (same as `date_start`).

- smooth_method:

  The method used to smooth annual demographic rates when converting
  them to a daily time scale. Must be one of the following:

  - **"none"**: No smoothing applied (annual values transposed to daily
    time scale).

  - **"linear"**: Applies simple moving average smoothing.

  - **"spline"**: Uses cubic spline interpolation for smoothing.

## Value

The function does not return a value. It estimates demographic rates and
saves the results as CSV files containing population size, birth rates,
and death rates over time.

## Details

The function performs the following steps:

1.  Filters the demographic data for the specified date range.

2.  Converts annual birth and death rates to daily estimates.

3.  Removes reported cholera deaths from all-cause mortality estimates.

4.  Applies the chosen smoothing method to daily demographic rates.

5.  Saves the processed data and model parameters to the specified
    directory.

The processed data file is saved in
`PATHS$DATA_PROCESSED/demographics/`, and the model input parameters are
stored in `PATHS$MODEL_INPUT/`.

Data Source: [United Nations World Population
Prospects](https://population.un.org/wpp/)

## Examples

``` r
if (FALSE) { # \dontrun{
# Define paths for raw and processed data
PATHS <- get_paths()

# Estimate demographic rates from 2000-01-01 to 2023-12-31 using spline smoothing
est_demographic_rates(PATHS, date_start = "2000-01-01", date_stop = "2023-12-31", smooth_method = "spline")
} # }
```
