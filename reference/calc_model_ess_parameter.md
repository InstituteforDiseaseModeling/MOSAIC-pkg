# Calculate Parameter-Specific ESS using Kernel Density Estimation

Computes the effective sample size (ESS) for individual parameters using
kernel density estimation to approximate marginal posteriors. This
method integrates out other parameters to provide a true measure of
information content for each parameter individually.

## Usage

``` r
calc_model_ess_parameter(
  results,
  param_names,
  likelihood_col = "likelihood",
  n_grid = 100,
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

  Integer number of grid points for KDE evaluation (default: 100)

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
