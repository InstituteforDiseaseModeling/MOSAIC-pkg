# Calculate Batch Size for Bookend Strategy

Predicts the number of simulations needed to reach a target ESS based on
the observed ESS trajectory. Fits sqrt and linear models to the
cumulative (n_sims, threshold_ESS) data and predicts from the sqrt model
unless the linear model fits clearly better. A log model is also fitted,
but only as a diagnostic (a warning when it fits best); it is never used
for prediction.

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

List with batch size recommendation. `phase` is one of `"complete"`,
`"low_confidence"`, `"no_progress"` or `"predictive"`.

## Details

The extrapolation is only defined when the chosen model's ESS increases
with the number of simulations. When its slope is zero or negative the
target is unreachable on the fitted trajectory, and the function returns
`phase = "no_progress"` with `batch_size = 0` instead of squaring a
negative root into a positive requirement.
