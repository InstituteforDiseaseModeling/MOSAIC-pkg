# Fit Posterior Distributions

Fits parametric distributions to NPE posterior samples for each
parameter. Creates proper nested structure with parameters_global and
parameters_location to match BFRS format for plotting.

## Usage

``` r
fit_posterior_distributions(
  posterior_samples,
  priors_file = NULL,
  output_file = NULL,
  verbose = TRUE
)
```

## Arguments

- posterior_samples:

  Matrix of posterior samples

- priors_file:

  Path to priors JSON file (for distribution types)

- output_file:

  Path to save fitted distributions

- verbose:

  Print progress

## Value

List of fitted distributions with proper global/location structure
