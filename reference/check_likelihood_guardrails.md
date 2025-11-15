# Check likelihood guardrails to identify unrealistic predictions

This function performs pre-screening checks on observed vs estimated
epidemic data to identify clearly unrealistic predictions that should
receive floor likelihood values. It checks for magnitude mismatches,
negative correlations, and nonsensical values.

## Usage

``` r
check_likelihood_guardrails(
  obs_cases,
  est_cases,
  obs_deaths,
  est_deaths,
  floor_ll = -1e+08,
  over_prediction_ratio = 50,
  under_prediction_ratio = 0.02,
  min_cases_for_epidemic = 500,
  substantial_epidemic = 2000,
  zero_threshold = 500,
  negative_correlation_threshold = -0.2,
  max_cases_per_timestep = 1e+06,
  max_deaths_per_timestep = 1e+05,
  cumulative_over_ratio = 500,
  cumulative_under_ratio = 0.001,
  min_cumulative_for_check = 100,
  verbose = FALSE
)
```

## Arguments

- obs_cases:

  Matrix of observed cases, `n_locations` x `n_time_steps`.

- est_cases:

  Matrix of estimated cases, same dimension as `obs_cases`.

- obs_deaths:

  Matrix of observed deaths, `n_locations` x `n_time_steps`.

- est_deaths:

  Matrix of estimated deaths, same dimension as `obs_deaths`.

- floor_ll:

  Numeric scalar, the floor likelihood value to return if guardrails
  triggered. Default `-1e8`.

- over_prediction_ratio:

  Numeric scalar, threshold for over-prediction detection. Triggers if
  estimated total \>= ratio \* observed total. Default `50`.

- under_prediction_ratio:

  Numeric scalar, threshold for severe under-prediction. Triggers if
  estimated total \< ratio \* observed total. Default `0.02`.

- min_cases_for_epidemic:

  Numeric scalar, minimum observed cases to trigger over-prediction
  check. Default `500`.

- substantial_epidemic:

  Numeric scalar, threshold for "missing epidemic" detection. Triggers
  under-prediction check when observed \> this value. Default `2000`.

- zero_threshold:

  Numeric scalar, minimum observed total to penalize zero predictions.
  Default `500`.

- negative_correlation_threshold:

  Numeric scalar, correlation threshold below which to trigger
  guardrail. Default `-0.2`.

- max_cases_per_timestep:

  Numeric scalar, maximum reasonable cases per location per timestep.
  Default `1e6`.

- max_deaths_per_timestep:

  Numeric scalar, maximum reasonable deaths per location per timestep.
  Default `1e5`.

- cumulative_over_ratio:

  Numeric scalar, threshold for cumulative over-prediction detection.
  Triggers if estimated cumulative \>= ratio \* observed cumulative.
  Default `500`.

- cumulative_under_ratio:

  Numeric scalar, threshold for severe cumulative under-prediction.
  Triggers if estimated cumulative \< ratio \* observed cumulative.
  Default `0.001`.

- min_cumulative_for_check:

  Numeric scalar, minimum observed cumulative to trigger ratio checks.
  Default `100`.

- verbose:

  Logical, if `TRUE` prints detailed violation messages. Default
  `FALSE`.

## Value

A list with components:

- `status`: Character, either `"PROCEED"` or `"FLOOR"`

- `violations`: Character vector describing any violations found

- `floor_ll`: The floor likelihood value (only if status is `"FLOOR"`)

## Details

The function performs three categories of checks:

1.  **Magnitude Mismatches**: Detects extreme over/under-prediction,
    missing epidemics, or zero predictions

2.  **Negative Correlations**: Identifies cases where predictions move
    opposite to observations

3.  **Nonsensical Values**: Catches negative, infinite, or extremely
    large predicted values

If any guardrail is triggered, the function returns `status = "FLOOR"`
indicating that `calc_model_likelihood` should return the floor
likelihood value immediately rather than performing expensive likelihood
calculations on clearly unrealistic predictions.

## Examples

``` r
# Example with over-prediction that should trigger guardrail
obs_cases <- matrix(c(10, 20, 15, 5), nrow = 2, ncol = 2)
est_cases <- matrix(c(200, 300, 250, 100), nrow = 2, ncol = 2)  # 15x over-prediction
obs_deaths <- matrix(c(1, 2, 1, 0), nrow = 2, ncol = 2)
est_deaths <- matrix(c(1, 2, 1, 0), nrow = 2, ncol = 2)

result <- check_likelihood_guardrails(obs_cases, est_cases, obs_deaths, est_deaths, verbose = TRUE)
print(result$status)  # Should be "FLOOR"
#> [1] "PROCEED"
```
