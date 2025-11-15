# Calculate log-likelihood for Gamma-distributed data

Computes the total log-likelihood for continuous positive data under the
Gamma distribution. The shape parameter is estimated via the method of
moments. Each observation can be weighted.

## Usage

``` r
calc_log_likelihood_gamma(observed, estimated, weights = NULL, verbose = TRUE)
```

## Arguments

- observed:

  Numeric vector of observed positive values.

- estimated:

  Numeric vector of expected means from the model (same length as
  `observed`).

- weights:

  Optional numeric vector of non-negative weights, same length as
  `observed`. Default is `NULL`, which sets all weights to 1.

- verbose:

  Logical; if `TRUE`, prints estimated shape parameter and total
  log-likelihood.

## Value

A scalar representing the total log-likelihood (numeric).

## Details

Weighted log-likelihood is summed over each observation. The shape
\\\\alpha\\ is estimated via method of moments from `observed`.

## Examples

``` r
calc_log_likelihood_gamma(c(2.5, 3.2, 1.8), c(2.4, 3.0, 2.0))
#> Gamma shape (α) = 12.76
#> Gamma log-likelihood: -1.73
#> [1] -1.731035
```
