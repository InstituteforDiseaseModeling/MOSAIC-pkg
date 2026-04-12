# Calculate Batch Size for Bookend Strategy

Predicts the number of simulations needed to reach a target ESS based on
the observed ESS trajectory. Fits sqrt, linear, and log models to the
cumulative (n_sims, threshold_ESS) data and returns a batch size.

## Usage

``` r
calc_bookend_batch_size(
  ess_history,
  target_ess,
  max_total_sims,
  target_r_squared = 0.95
)
```

## Arguments

- ess_history:

  ESS measurements from calibration phase

- target_ess:

  Target ESS value

- max_total_sims:

  Maximum total simulations allowed

- target_r_squared:

  Target R-squared for ESS regression (default: 0.95)

## Value

List with batch size recommendation
