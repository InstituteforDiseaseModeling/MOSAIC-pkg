# Dispatcher for multiple distribution log-likelihood functions

This function routes to the appropriate `calc_log_likelihood_*()`
sub-function based on the specified `family`. It supports the six
distributions (Beta, Binomial, Gamma, Negative Binomial, Normal,
Poisson) and also allows a top-level `weights` argument that is passed
down to the chosen sub-function.

## Usage

``` r
calc_log_likelihood(
  observed,
  estimated,
  family = c("beta", "binomial", "gamma", "negbin", "normal", "poisson"),
  weights = NULL,
  ...
)
```

## Arguments

- observed:

  Numeric or integer vector of observed data (depends on distribution).
  NAs allowed.

- estimated:

  Numeric vector of model-predicted values (depends on distribution).
  NAs allowed.

- family:

  Character string, one of:

  - `"beta"`

  - `"binomial"`

  - `"gamma"`

  - `"negbin"`

  - `"normal"`

  - `"poisson"`

- weights:

  Optional numeric vector of non-negative weights, same length as
  `observed`. Default is `NULL`, which sets all weights to 1.

- ...:

  Additional arguments passed to the underlying distribution function:

  - `mean_precision` (for
    [`calc_log_likelihood_beta`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_log_likelihood_beta.md))

  - `trials` (for
    [`calc_log_likelihood_binomial`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_log_likelihood_binomial.md))

  - `k`, `k_min` (for
    [`calc_log_likelihood_negbin`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_log_likelihood_negbin.md))

  - `verbose` (common to all distributions)

## Value

The total log-likelihood (scalar) for most families, or (in the case of
Normal) a list with `log_likelihood`, `sigma`, `shapiro_p`.

## Details

Based on the value of `family`, this function internally calls:

- [`calc_log_likelihood_beta`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_log_likelihood_beta.md)
  for `family = "beta"`

- [`calc_log_likelihood_binomial`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_log_likelihood_binomial.md)
  for `family = "binomial"`

- [`calc_log_likelihood_gamma`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_log_likelihood_gamma.md)
  for `family = "gamma"`

- [`calc_log_likelihood_negbin`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_log_likelihood_negbin.md)
  for `family = "negbin"`

- [`calc_log_likelihood_normal`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_log_likelihood_normal.md)
  for `family = "normal"`

- [`calc_log_likelihood_poisson`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_log_likelihood_poisson.md)
  for `family = "poisson"`

The `weights` argument (if not `NULL`) is passed to the chosen
sub-likelihood function, which multiplies each observation's
log-likelihood contribution by `weights[i]`.

## See also

[`calc_log_likelihood_beta`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_log_likelihood_beta.md),
[`calc_log_likelihood_binomial`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_log_likelihood_binomial.md),
[`calc_log_likelihood_gamma`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_log_likelihood_gamma.md),
[`calc_log_likelihood_negbin`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_log_likelihood_negbin.md),
[`calc_log_likelihood_normal`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_log_likelihood_normal.md),
[`calc_log_likelihood_poisson`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_log_likelihood_poisson.md)

## Examples

``` r
# Poisson example with weights
calc_log_likelihood(
  observed  = c(2, 3, 4),
  estimated = c(2.2, 2.9, 3.8),
  family    = "poisson",
  weights   = c(1, 2, 1),
  verbose   = TRUE
)
#> Poisson log-likelihood: -5.95
#> [1] -5.949537

# Negative Binomial with known k and weights
calc_log_likelihood(
  observed  = c(0, 5, 9),
  estimated = c(3, 4, 5),
  family    = "negbin",
  k         = 2.0,
  weights   = c(1, 1, 2),
  verbose   = TRUE
)
#> Using provided k = 2.000
#> k = 2.000 < k_min = 3.000; using k_min.
#> Negative Binomial log-likelihood (k=3.000): -10.71
#> [1] -10.70527
```
