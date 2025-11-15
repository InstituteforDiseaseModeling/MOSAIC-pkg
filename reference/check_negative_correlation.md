# Check for negative correlations between observed and estimated time series

Check for negative correlations between observed and estimated time
series

## Usage

``` r
check_negative_correlation(
  obs_cases,
  est_cases,
  obs_deaths,
  est_deaths,
  threshold
)
```

## Arguments

- obs_cases:

  Matrix of observed cases

- est_cases:

  Matrix of estimated cases

- obs_deaths:

  Matrix of observed deaths

- est_deaths:

  Matrix of estimated deaths

- threshold:

  Correlation threshold below which to trigger guardrail

## Value

Character vector of violations found
