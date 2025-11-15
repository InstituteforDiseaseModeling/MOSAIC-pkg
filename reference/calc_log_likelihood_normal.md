# Calculate log-likelihood for Normally-distributed continuous data

Computes the total log-likelihood for continuous data under the Normal
distribution. Also performs the Shapiro-Wilk test to check normality of
residuals. Each observation can be weighted via `weights`.

## Usage

``` r
calc_log_likelihood_normal(observed, estimated, weights = NULL, verbose = TRUE)
```

## Arguments

- observed:

  Numeric vector of observed continuous values.

- estimated:

  Numeric vector of model-predicted means.

- weights:

  Optional numeric vector of non-negative weights, same length as
  `observed`. Default is `NULL`, which sets all weights to 1.

- verbose:

  Logical; if `TRUE`, prints estimated sigma, Shapiro-Wilk p-value, and
  log-likelihood.

## Value

A single numeric value representing the total log-likelihood.

## Details

Weighted log-likelihood is computed across all observations, and the
residuals are tested for normality via the Shapiro-Wilk test:

- If `p < 0.05`, a warning is issued indicating non-normal residuals.

- Otherwise, if `verbose = TRUE`, a message is printed showing the
  p-value.

## Examples

``` r
ll <- calc_log_likelihood_normal(c(1.2, 2.8, 3.1), c(1.0, 3.0, 3.2))
#> Shapiro-Wilk p = 0.4633: residuals are consistent with normality.
#> Estimated σ = 0.2082
#> Normal log-likelihood: 0.91
print(ll)
#> [1] 0.9129725
```
