# Calculate Batch Size for Bookend Strategy

Implements a three-phase strategy:

1.  Initial calibration (n_batches × batch_size)

2.  One large predictive batch (calculated from ESS rate)

3.  Final fine-tuning (adaptive batch sizing based on gap)

## Usage

``` r
calc_bookend_batch_size(
  ess_history,
  target_ess,
  reserved_sims,
  max_total_sims,
  target_r_squared = 0.95
)
```

## Arguments

- ess_history:

  ESS measurements from calibration phase

- target_ess:

  Target ESS value

- reserved_sims:

  Number of simulations reserved for fine-tuning

- max_total_sims:

  Maximum total simulations allowed

- target_r_squared:

  Target R-squared for ESS regression (default: 0.95)

## Value

List with batch size recommendation
