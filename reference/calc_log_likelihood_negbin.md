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
  k_min = NULL,
  weights = NULL,
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

- k:

  Numeric scalar; dispersion parameter. If `NULL`, it is estimated via
  method of moments.

- k_min:

  Deprecated and ignored; retained only so existing calls do not error.
  Dispersion is estimated by
  [`est_nb_dispersion`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_nb_dispersion.md)
  and arrives already bounded. If `k = Inf` (Poisson limit), no flooring
  is applied.

- weights:

  Optional numeric vector of non-negative weights, same length as
  `observed`. Default is `NULL`, which sets all weights to 1.

- eps_rel:

  Positive scalar; the predicted mean of every cell is floored at
  `max(1e-4, eps_rel * mean(observed))` before the density is evaluated.
  Default `0.02`. This floor is the only thing standing between a zero
  prediction and `log(0)`, and its SIZE sets how hard a spurious zero is
  punished, so it is channel-specific: see
  [`calc_model_likelihood`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_likelihood.md)
  (`eps_rel_cases` / `eps_rel_deaths`).

- verbose:

  Logical; if `TRUE`, prints diagnostics including the (floored)
  dispersion and total log-likelihood.

## Value

A scalar representing the total log-likelihood (numeric).

## Details

If `k` is not supplied, it is estimated as \\k = \bar{x}^2 / (s^2 -
\bar{x})\\ from `observed`. If \\s^2 \le \bar{x}\\, the function uses
the Poisson limit (`k = Inf`).

## Examples

``` r
# k is used as supplied
calc_log_likelihood_negbin(c(0, 5, 9), c(3, 4, 5))
#> Estimated k = 1.390 (from Var = 20.333, Mean = 4.667)
#> Negative Binomial log-likelihood (k=1.390): -7.50
#> [1] -7.498757
# Supply the dispersion k explicitly (used exactly as given)
calc_log_likelihood_negbin(c(0, 5, 9), c(3, 4, 5), k = 1.2)
#> Using provided k = 1.200
#> Negative Binomial log-likelihood (k=1.200): -7.51
#> [1] -7.513423
```
