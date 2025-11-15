# Estimate Seasonal Dynamics for Cholera and Precipitation Using Daily Fourier Series

This function retrieves historical precipitation data, processes cholera
case data, and fits seasonal dynamics models using a double Fourier
series at the daily scale (p = 365). Weekly data is expanded by
assigning each weekly value to every day in that week.

## Usage

``` r
est_seasonal_dynamics(
  PATHS,
  date_start,
  date_stop,
  min_obs = 30,
  clustering_method,
  k,
  exclude_iso_codes = NULL,
  data_sources = c("WHO", "JHU", "SUPP")
)
```

## Arguments

- PATHS:

  A list containing paths where raw and processed data are stored. See
  [`get_paths()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_paths.md).

- date_start:

  A date in YYYY-MM-DD format indicating the start date of the
  precipitation data.

- date_stop:

  A date in YYYY-MM-DD format indicating the stop date of the
  precipitation data.

- min_obs:

  The minimum number of observations required to fit the Fourier series
  to cholera case data.

- clustering_method:

  The clustering method to use (e.g., "kmeans", "ward.D2", etc.).

- k:

  Number of clusters for grouping countries by seasonality.

- exclude_iso_codes:

  Optional character vector of ISO codes to exclude from clustering and
  neighbor matching.

- data_sources:

  Character vector of data sources to include. Default is c('WHO',
  'JHU', 'SUPP').

## Value

Saves daily fitted values and parameter estimates to CSV.
