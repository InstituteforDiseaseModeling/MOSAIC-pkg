# Calculate log-likelihood for Negative Binomial-distributed count data

Computes the total log-likelihood for count data under the Negative
Binomial distribution, using the gamma-function formulation. Each
observation can be weighted.

## Usage

``` r
calc_log_likelihood_negbin(
  observed,
  estimated,
  k = NULL,
  k_min = 3,
  weights = NULL,
  verbose = TRUE
)
```

## Arguments

- observed:

  Integer vector of observed non-negative counts (e.g., cases, deaths).

- estimated:

  Numeric vector of expected values from the model (same length as
  `observed`).

- k:

  Numeric scalar; dispersion parameter. If `NULL`, it is estimated via
  method of moments.

- k_min:

  Numeric scalar; minimum dispersion floor applied when `k` is finite
  (either supplied or estimated). Default `3`. If `k = Inf` (Poisson
  limit), no flooring is applied.

- weights:

  Optional numeric vector of non-negative weights, same length as
  `observed`. Default is `NULL`, which sets all weights to 1.

- verbose:

  Logical; if `TRUE`, prints diagnostics including the (floored)
  dispersion and total log-likelihood.

## Value

A scalar representing the total log-likelihood (numeric).

## Details

If `k` is not supplied, it is estimated as \\k = \bar{x}^2 / (s^2 -
\bar{x})\\ from `observed`. When this estimate is finite, it is
constrained to be at least `k_min`. If \\s^2 \le \bar{x}\\, the function
uses the Poisson limit (`k = Inf`).

## Examples

``` r
# Default k_min = 3
calc_log_likelihood_negbin(c(0, 5, 9), c(3, 4, 5))
#> Estimated k = 1.390 (from Var = 20.333, Mean = 4.667)
#> k = 1.390 < k_min = 3.000; using k_min.
#> Negative Binomial log-likelihood (k=3.000): -7.54
#> [1] -7.540079
# Provide k but allow flooring if too small
calc_log_likelihood_negbin(c(0, 5, 9), c(3, 4, 5), k = 1.2)
#> Using provided k = 1.200
#> k = 1.200 < k_min = 3.000; using k_min.
#> Negative Binomial log-likelihood (k=3.000): -7.54
#> [1] -7.540079
```
