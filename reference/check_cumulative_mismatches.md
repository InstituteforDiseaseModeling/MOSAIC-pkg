# Check for cumulative case and death mismatches

Check for cumulative case and death mismatches

## Usage

``` r
check_cumulative_mismatches(
  obs_cases,
  est_cases,
  obs_deaths,
  est_deaths,
  over_ratio,
  under_ratio,
  min_cumulative
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

  Threshold for cumulative over-prediction detection

- under_ratio:

  Threshold for severe cumulative under-prediction

- min_cumulative:

  Minimum observed cumulative to trigger ratio checks

## Value

Character vector of violations found
