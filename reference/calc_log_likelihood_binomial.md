# Calculate log-likelihood for Binomial-distributed data

Computes the total log-likelihood for integer counts of successes under
the Binomial distribution, optionally weighting each observation via
`weights`.

## Usage

``` r
calc_log_likelihood_binomial(
  observed,
  estimated,
  trials,
  weights = NULL,
  verbose = TRUE
)
```

## Arguments

- observed:

  Integer vector of observed successes (e.g., number of positives).

- estimated:

  Numeric vector of expected probabilities of success in (0, 1).

- trials:

  Integer vector of total trials (same length as `observed`).

- weights:

  Optional numeric vector of non-negative weights, same length as
  `observed`. Default is `NULL`, which sets all weights to 1.

- verbose:

  Logical; if `TRUE`, prints total log-likelihood and checks for data
  consistency.

## Value

A scalar log-likelihood (numeric).

## Details

The Binomial distribution models the number of successes in fixed
trials: \$\$ Y \\sim \\text{Binomial}(n, p). \$\$ Weighted by
`weights[i]` for each observation `i`.

## Examples

``` r
calc_log_likelihood_binomial(c(3, 4, 2), c(0.3, 0.5, 0.25), c(10, 10, 8))
#> Binomial log-likelihood: -4.07
#> [1] -4.071992
```
