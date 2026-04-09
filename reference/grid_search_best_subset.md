# Grid Search for Best Subset with Early Stopping

Performs exhaustive grid search to find the smallest subset size that
meets convergence criteria (ESS, A, CVw) using Gibbs weighting. Stops at
first convergence.

## Usage

``` r
grid_search_best_subset(
  results,
  target_ESS,
  target_A,
  target_CVw,
  min_size,
  max_size,
  step_size = 1,
  ess_method = c("kish", "perplexity"),
  verbose = FALSE
)
```

## Arguments

- results:

  Data frame of calibration results with columns: sim, likelihood

- target_ESS:

  Numeric target for Effective Sample Size (ESS)

- target_A:

  Numeric target for Agreement Index (A)

- target_CVw:

  Numeric target for Coefficient of Variation of weights (CVw)

- min_size:

  Integer minimum subset size to search

- max_size:

  Integer maximum subset size to search

- step_size:

  Integer step size for search (default 1)

- ess_method:

  Character ESS calculation method: "kish" or "perplexity"

- verbose:

  Logical print progress messages

## Value

List with elements:

- n: Optimal subset size (smallest n meeting criteria)

- subset: Data frame of selected simulations

- metrics: List with ESS, A, CVw values at optimal n

- converged: Logical indicating if criteria were met

- evaluations: Integer number of n values tested

## Details

The function searches from min_size to max_size by step_size, stopping
at the first size where all three criteria are met simultaneously:

- ESS \>= target_ESS

- A \>= target_A

- CVw \<= target_CVw

Metrics are calculated using Gibbs weighting with dynamic temperature:

1.  Calculate AIC = -2 \* likelihood for subset

2.  Calculate Delta AIC (relative to best in subset)

3.  Calculate dynamic temperature:

    - effective_range = 4.0 (standard Akaike range)

    - actual_range = range(Delta AIC)

    - temperature = 0.5 \* (effective_range / actual_range)

4.  Apply Gibbs weighting: weights = exp(-Delta AIC / temperature) / Z

5.  Calculate ESS, A, CVw from Gibbs-weighted samples

The temperature adapts to the data's actual Delta AIC range, providing
consistent discrimination across different likelihood spreads.

If no size meets criteria, returns results at max_size with
converged=FALSE.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- grid_search_best_subset(
  results = calibration_results,
  target_ESS = 500,
  target_A = 0.95,
  target_CVw = 0.7,
  min_size = 30,
  max_size = 1000
)
} # }
```
