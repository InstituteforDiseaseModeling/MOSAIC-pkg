# Process Environmental Suitability Data for Cholera Transmission

This function loads climate data, ENSO data, and weekly cholera cases
data, processes them, and merges them into a dataset to estimate the
environmental suitability for cholera transmission based on various
environmental factors. It also creates a binary environmental
suitability indicator based on case patterns.

## Usage

``` r
process_suitability_data(
  PATHS,
  cutoff,
  use_epidemic_peaks = FALSE,
  date_start = NULL,
  date_stop = NULL,
  forecast_mode = TRUE,
  forecast_horizon = 3,
  include_lags = FALSE
)
```

## Arguments

- PATHS:

  A list containing paths where the data is stored. Typically generated
  by the
  [`get_paths()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_paths.md)
  function and should include:

  - **DATA_CLIMATE**: Path to the directory where processed climate data
    is stored.

  - **DATA_ENSO**: Path to the directory where ENSO data is stored.

  - **DATA_CHOLERA_WEEKLY**: Path to the directory containing processed
    combined weekly cholera cases data (WHO+JHU+SUPP sources).

- cutoff:

  Numeric threshold for case counts. Used when use_epidemic_peaks =
  FALSE. Weeks with cases \>= cutoff are considered outbreak periods for
  the environmental suitability calculation. Default behavior uses this
  value to create the binary indicator.

- use_epidemic_peaks:

  Logical. If TRUE, uses epidemic peak periods from est_epidemic_peaks()
  to define environmental suitability using exact peak_start to
  peak_stop dates. Provides deterministic outbreak detection without any
  modifications or lead-up periods. Default FALSE for backwards
  compatibility.

- date_start:

  Optional start year for data filtering. If NULL, automatically
  determined from case data (one year prior to earliest case
  observation).

- date_stop:

  Optional end year for data filtering. If NULL, automatically
  determined from latest ENSO data availability.

- forecast_mode:

  Logical. If TRUE, only includes variables that can support forecasting
  at the specified horizon. Excludes case-dependent epidemic memory and
  spatial import pressure variables. Default TRUE.

- forecast_horizon:

  Numeric. Forecasting horizon in months. Variables dependent on
  surveillance data beyond this horizon are excluded in forecast mode.
  Default 3 months.

- include_lags:

  Logical. If TRUE, includes time-lagged climate variables with
  epidemiologically-informed lag periods specific to each variable.
  Default FALSE.

## Value

This function processes the data and merges the climate, ENSO, and
cholera cases data into a single dataset. It creates a `cases_binary`
column indicating environmental suitability based on case patterns using
sophisticated temporal logic. The processed dataset is saved as a CSV
file.

## Details

The function performs the following key steps:

- Loads and merges climate data (temperature, precipitation, etc.) from
  multiple countries

- Loads ENSO and DMI climate index data

- Loads combined weekly cholera case surveillance data from WHO, JHU,
  and supplemental sources

- Merges all datasets by country, year, and week

- Creates environmental suitability indicator (`cases_binary`) using
  either epidemic peak-based or threshold-based methods

- Saves the complete merged dataset for use in environmental suitability
  modeling

**Environmental Suitability Methods:**

- **Epidemic Peak-based** (use_epidemic_peaks = TRUE): Uses
  deterministic outbreak detection from est_epidemic_peaks() using exact
  peak_start to peak_stop dates without any modifications

- **Threshold-based** (use_epidemic_peaks = FALSE): Uses simple case
  count cutoffs with temporal lead-up logic

The epidemic peak-based method provides deterministic, epidemiologically
meaningful outbreak periods and is recommended for consistent
environmental suitability modeling.

## See also

[`get_cases_binary`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_cases_binary.md)
for the threshold-based environmental suitability method
[`get_cases_binary_from_peaks`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_cases_binary_from_peaks.md)
for the epidemic peak-based method
[`est_epidemic_peaks`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_epidemic_peaks.md)
for epidemic peak detection
