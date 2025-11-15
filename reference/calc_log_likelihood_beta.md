# Calculate log-likelihood for Beta-distributed proportions

Computes the total log-likelihood for proportion data under the Beta
distribution. Supports either the mean–precision parameterization
(default) or the standard shape parameterization. In both cases, a
single global set of shape parameters is estimated from the data using
the method of moments. Each observation can be weighted via `weights`.

## Usage

``` r
calc_log_likelihood_beta(
  observed,
  estimated,
  mean_precision = TRUE,
  weights = NULL,
  verbose = TRUE
)
```

## Arguments

- observed:

  Numeric vector of observed values in (0, 1).

- estimated:

  Numeric vector of model-predicted values in (0, 1).

- mean_precision:

  Logical; if `TRUE` (default), use mean–precision parameterization. If
  `FALSE`, estimate shape parameters directly from the observed vector.

- weights:

  Optional numeric vector of non-negative weights, same length as
  `observed`. Default is `NULL`, which sets all weights to 1.

- verbose:

  Logical; if `TRUE`, prints shape parameter estimates and total
  log-likelihood.

## Value

A scalar log-likelihood (numeric).

## Details

When `mean_precision = TRUE`, the precision parameter \\\phi\\ is
estimated as: \$\$ \phi = \frac{\mu (1 - \mu)}{\text{Var}(Y)} - 1 \$\$
Then: \$\$ \text{shape}\_1 = \mu_t \phi, \quad \text{shape}\_2 = (1 -
\mu_t) \phi \$\$

When `mean_precision = FALSE`, a single global \\\text{shape}\_1\\,
\\\text{shape}\_2\\ is estimated from the observed vector: \$\$
\text{shape}\_1 = \left( \frac{1 - \mu}{\sigma^2} - \frac{1}{\mu}
\right) \mu^2 \$\$ \$\$ \text{shape}\_2 = \text{shape}\_1 \left(
\frac{1}{\mu} - 1 \right) \$\$

The total log-likelihood is then summed across observations, each
multiplied by `weights[i]`.

## Examples

``` r
calc_log_likelihood_beta(c(0.2, 0.6, 0.4), c(0.25, 0.55, 0.35))
#> Mean–precision mode: estimated phi = 71.00
#> Beta log-likelihood: 4.77
#> [1] 4.770705
```
