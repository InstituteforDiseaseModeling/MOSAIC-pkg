# Calculate log-likelihood for Poisson-distributed count data

Computes the total log-likelihood for integer count data under the
Poisson distribution. Each observation can be weighted via `weights`,
defaulting to equal weights.

## Usage

``` r
calc_log_likelihood_poisson(
  observed,
  estimated,
  weights = NULL,
  zero_buffer = TRUE,
  eps_rel = 0.02,
  verbose = TRUE
)
```

## Arguments

- observed:

  Integer vector of observed non-negative counts (e.g., cases, deaths).

- estimated:

  Numeric vector of expected values from the model (same length as
  `observed`).

- weights:

  Optional numeric vector of non-negative weights, same length as
  `observed`. Default is `NULL`, which sets all weights to 1.

- zero_buffer:

  Logical; if `TRUE` (default), rounds observed values to integers and
  adds small buffer to avoid zero estimates. If `FALSE`, enforces strict
  integer requirements.

- eps_rel:

  Positive scalar; the predicted mean of every cell is floored at
  `max(1e-4, eps_rel * mean(observed))` before the density is evaluated.
  Default `0.02`. See
  [`calc_log_likelihood_negbin`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_log_likelihood_negbin.md).

- verbose:

  Logical; if `TRUE`, prints diagnostics and total log-likelihood.

## Value

A scalar representing the total log-likelihood (numeric).

## Details

The Poisson distribution assumes that the variance equals the mean,
\\\mathrm{Var}(Y) = \mu\\. Weighted log-likelihood is summed across
observations. If the variance/mean ratio is \> 1.5, a warning about
possible overdispersion is issued.

## Examples

``` r
calc_log_likelihood_poisson(c(2, 3, 4), c(2.2, 2.9, 4.1))
#> Poisson log-likelihood: -4.45
#> [1] -4.447966
```
