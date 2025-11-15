# Check for magnitude mismatches between observed and estimated data

Check for magnitude mismatches between observed and estimated data

## Usage

``` r
check_magnitude_mismatches(
  obs_cases,
  est_cases,
  obs_deaths,
  est_deaths,
  over_ratio,
  under_ratio,
  min_cases,
  substantial,
  zero_thresh
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

- over_ratio:

  Threshold for over-prediction detection

- under_ratio:

  Threshold for under-prediction detection

- min_cases:

  Minimum observed cases to trigger over-prediction check

- substantial:

  Threshold for missing epidemic detection

- zero_thresh:

  Minimum observed to penalize zero predictions

## Value

Character vector of violations found
