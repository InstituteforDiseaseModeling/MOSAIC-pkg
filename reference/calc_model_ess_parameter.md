# Calculate Parameter-Specific ESS

Computes the effective sample size (ESS) for individual parameters using
one of three methods: KDE-based marginal posterior estimation, Owen's
integrand-specific ESS, or binned marginal Kish ESS.

## Usage

``` r
calc_model_ess_parameter(
  results,
  param_names,
  likelihood_col = "likelihood",
  n_grid = 100,
  method = c("kish", "perplexity"),
  marginal_method = c("kde", "owen", "binned"),
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

- n_grid:

  Integer number of grid points for KDE evaluation (default: 100, used
  only by "kde" method)

- method:

  Character string specifying ESS formula: "kish" or "perplexity"

- marginal_method:

  Character string specifying how marginal weights are constructed:
  "kde" (default, backward-compatible KDE-based), "owen" (Owen's
  integrand-specific ESS), or "binned" (binned marginal Kish ESS).
  Owen's and binned methods are more sensitive to importance weight
  changes.

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
