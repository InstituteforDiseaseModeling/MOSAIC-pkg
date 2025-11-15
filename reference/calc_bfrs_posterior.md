# Calculate BFRS Posterior via Importance Sampling

Creates a posterior approximation from BFRS simulation results using
importance sampling with likelihood weights.

## Usage

``` r
calc_bfrs_posterior(
  results,
  param_names,
  n_samples = 10000,
  method = "importance_sampling",
  log_likelihood = TRUE,
  temperature = 1,
  verbose = FALSE
)
```

## Arguments

- results:

  Data frame containing simulation results with parameters and
  likelihoods

- param_names:

  Character vector of parameter names to include in posterior

- n_samples:

  Integer number of posterior samples to generate (default 10000)

- method:

  Character string specifying method: "importance_sampling" or
  "rejection_sampling"

- log_likelihood:

  Logical whether likelihood column is in log scale (default TRUE)

- temperature:

  Numeric temperature parameter for tempering (default 1.0)

- verbose:

  Logical whether to print progress messages

## Value

List containing:

- samples:

  Matrix of posterior samples (n_samples × n_params)

- weights:

  Vector of importance weights for original simulations

- log_weights:

  Log-scale importance weights

- ess:

  Effective sample size

- mean:

  Posterior mean estimates

- cov:

  Posterior covariance matrix

- quantiles:

  Posterior quantiles at standard levels

- log_evidence:

  Log marginal likelihood estimate

## Examples

``` r
if (FALSE) { # \dontrun{
# After running BFRS simulations
bfrs_posterior <- calc_bfrs_posterior(
    results = results,
    param_names = c("tau_i_ETH", "mobility_gamma_ETH", "mu_j_ETH"),
    n_samples = 10000,
    verbose = TRUE
)
} # }
```
