# Identify Best Subset of Simulations for NPE Prior Definition

Uses binary search on percentile to find the smallest subset of top
simulations (by likelihood) that meets convergence criteria for ESS_B,
A, CVw, and minimum size. This subset will be used to define priors for
Neural Posterior Estimation (NPE).

## Usage

``` r
identify_best_subset(
  results,
  min_B = 100,
  target_ESS_B = 50,
  target_A = 0.95,
  target_CVw = 0.5,
  min_percentile = 0.001,
  max_percentile = 30,
  precision = 0.1,
  verbose = FALSE
)
```

## Arguments

- results:

  Data frame containing simulation results with columns:

  - sim: Simulation ID

  - likelihood: Log-likelihood values

  - Other columns containing parameter values

- min_B:

  Integer minimum number of simulations to include even if percentile is
  smaller (default: 100)

- target_ESS_B:

  Numeric target minimum ESS for the subset using perplexity method
  (default: 50)

- target_A:

  Numeric target minimum agreement index (default: 0.95)

- target_CVw:

  Numeric target maximum coefficient of variation (default: 0.5)

- min_percentile:

  Numeric minimum percentile to consider (default: 0.001) The search
  will not consider subsets smaller than this percentile. E.g., 0.001
  means at least the top 0.001% will be considered. Note: The actual
  minimum subset size is max(min_percentile%, min_B simulations).

- max_percentile:

  Numeric maximum percentile to consider (default: 30) The search will
  not consider subsets larger than this percentile. E.g., 30 means at
  most the top 30% will be considered.

- precision:

  Numeric precision for binary search convergence as a percentage
  (default: 0.1) The search stops when the range is smaller than this
  value.

- verbose:

  Logical whether to print progress messages (default: FALSE)

## Value

A list containing:

- subset: Data frame of selected simulations

- metrics: Final convergence metrics (ESS_B, A, CVw, B_size)

- converged: Logical indicating if all criteria were met

- n_selected: Number of simulations in final subset

- percentile_used: Final percentile of simulations selected

## Examples

``` r
if (FALSE) { # \dontrun{
# Identify best subset from calibration results
best_subset <- identify_best_subset(
  results = calibration_results,
  min_B = 100,
  target_ESS_B = 50,
  target_A = 0.95,
  target_CVw = 0.5,
  min_percentile = 0.001,  # Search down to top 0.001%
  max_percentile = 30,     # Don't consider more than top 30%
  verbose = TRUE
)

# Use subset for NPE prior definition
npe_priors <- estimate_priors_from_subset(best_subset$subset)
} # }
```
