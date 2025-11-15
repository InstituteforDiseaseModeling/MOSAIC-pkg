# Evaluate Prior Densities for SMC

Evaluates log p(theta) for each posterior sample using original priors.
Required for SMC importance weight calculation.

## Usage

``` r
evaluate_prior_densities(samples, priors, verbose = FALSE)
```

## Arguments

- samples:

  Matrix of parameter samples

- priors:

  List of prior specifications

- verbose:

  Print progress

## Value

Vector of log prior densities
