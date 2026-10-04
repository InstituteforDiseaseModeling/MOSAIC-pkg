# Shrink per-location dispersion toward a mean-dispersion trend

Empirical-Bayes shrinkage in the style of DESeq2 (Love, Huber & Anders
2014): \\\log k_j \sim N(\log k\_{trend}(\mu_j), \sigma_p^2)\\. Each
location's own estimate is weighted by its PRECISION, \\w_j = \sigma_p^2
/ (\sigma_p^2 + s_j^2)\\ with \\s_j = se_j / k_j\\ (delta method), and
the prior variance is recovered by the DESeq2 variance decomposition
\\\sigma_p^2 = Var(resid) - \overline{s_j^2}\\. A flat blend would be
the posterior mean only if sampling variance equalled the prior
variance. Locations with no estimate inherit the trend; locations with
no positive mean fall back to Poisson.

## Usage

``` r
.nb_disp_shrink(mean_weekly, k, se = NULL, identified = NULL, clamped = NULL)
```

## Arguments

- mean_weekly:

  Numeric vector of per-location weekly means.

- k:

  Numeric vector of per-location dispersion estimates (may be `NA` or
  `Inf`).

- se:

  Numeric vector of standard errors on `k`, used to weight each
  location's own estimate by its precision.

- identified:

  Logical vector; unidentified estimates carry no usable precision and
  lean on the trend.

- clamped:

  Logical vector; estimates censored at or near the lower bound
  ([`.nb_disp_censored()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/dot-nb_disp_censored.md))
  are shrunk toward the trend but excluded from fitting it.

## Value

List with `k` (shrunk vector), `trend`, `sigma` and counts.
