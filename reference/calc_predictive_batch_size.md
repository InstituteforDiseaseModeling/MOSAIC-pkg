# Calculate Predictive Batch Size for Near-Complete Convergence

After calibration phase, predicts the number of simulations needed to
reach near-convergence (e.g., 90-95% of target) in a single large batch.

## Usage

``` r
calc_predictive_batch_size(
  ess_history,
  target_ess,
  safety_factor = 0.9,
  max_batch_size = 10000,
  confidence_threshold = 0.7
)
```

## Arguments

- ess_history:

  List of ESS measurements from calibration phase

- target_ess:

  Target ESS value for convergence

- safety_factor:

  Factor to apply (e.g., 0.9 = aim for 90% of needed sims)

- max_batch_size:

  Maximum allowed batch size

- confidence_threshold:

  Minimum R-squared for prediction (default: 0.7)

## Value

List with recommended batch size and prediction details
