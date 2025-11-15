# Generate Synthetic Data for Testing calc_model_ess_parameter

Generate Synthetic Data for Testing calc_model_ess_parameter

## Usage

``` r
generate_test_data_for_ess(
  n_samples = 2000,
  n_params = 5,
  true_ess = c(1500, 1000, 500, 200, 100)
)
```

## Arguments

- n_samples:

  Number of samples to generate

- n_params:

  Number of parameters

- true_ess:

  Target effective sample size pattern

## Value

Data frame with synthetic results
