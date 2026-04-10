# Calculate Parameter-Specific ESS

Computes the effective sample size (ESS) for individual parameters using
one of two methods: binned marginal Kish ESS (default) or KDE-based
marginal posterior estimation.

## Usage

``` r
calc_model_ess_parameter(
  results,
  param_names,
  likelihood_col = "likelihood",
  n_bins = 100,
  n_grid = 100,
  method = c("kish", "perplexity"),
  marginal_method = c("kde", "binned"),
  verbose = FALSE
)
```

## Arguments

- results:

  Data frame containing simulation results

- param_names:

  Character vector of parameter names to analyze (required)

- likelihood_col:

  Character name of the column containing log-likelihood values
  (default: "likelihood")

- n_bins:

  Integer number of bins for the binned method, or NULL for adaptive
  sqrt(n) scaling. Fixed bin count (e.g. 100) removes sample-size
  dependence from ESS estimates. Default: 100. Only used by "binned"
  method.

- n_grid:

  Integer number of grid points for KDE evaluation (default: 100, used
  only by "kde" method)

- method:

  Character string specifying ESS formula: "kish" or "perplexity"

- marginal_method:

  Character string specifying how marginal weights are constructed:
  "kde" (default, KDE-based marginal posterior estimation) or "binned"
  (more conservative, directly sensitive to importance weight
  distribution — recommended for final production runs).

- verbose:

  Logical whether to print progress messages (default: FALSE)

## Value

Data frame with columns:

- parameter: Parameter name

- type: Parameter type ('global' or 'location')

- iso_code: ISO code for location parameters (NA for global)

- ess_marginal: Marginal ESS for this parameter

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic usage with simulation results
ess_results <- calc_model_ess_parameter(
  results = simulation_results,
  param_names = c("tau_i", "mu_j", "gamma_2")
)

# With custom likelihood column name
ess_results <- calc_model_ess_parameter(
  results = simulation_results,
  param_names = c("tau_i", "mu_j", "gamma_2"),
  likelihood_col = "log_lik"
)

} # }
```
