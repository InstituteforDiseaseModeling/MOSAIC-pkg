# Convert BFRS Posterior to Prior Format

Converts a BFRS posterior object into a prior specification that can be
used with sample_parameters() for sequential Bayesian updating.

## Usage

``` r
bfrs_posterior_to_prior(bfrs_posterior, method = "gaussian")
```

## Arguments

- bfrs_posterior:

  BFRS posterior object from calc_bfrs_posterior()

- method:

  Character string: "gaussian", "kde", or "empirical"

## Value

List in prior specification format
