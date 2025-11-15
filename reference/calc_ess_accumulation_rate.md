# Calculate ESS Accumulation Rate

Tracks the rate at which ESS accumulates per simulation and predicts the
number of additional simulations needed to reach target ESS.

## Usage

``` r
calc_ess_accumulation_rate(ess_history, target_ess, min_points = 3)
```

## Arguments

- ess_history:

  List of ESS measurements at different simulation counts

- target_ess:

  Target ESS value for convergence

- min_points:

  Minimum number of data points needed for prediction (default: 3)

## Value

List containing accumulation metrics and predictions
