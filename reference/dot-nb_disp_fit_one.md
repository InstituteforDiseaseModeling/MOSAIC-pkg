# Fit the conditional dispersion for one location

Descends a ladder of progressively simpler mean models. A high-degree
spline on a series with long runs of zeros drives fitted rates to zero
and breaks the IRLS ("NA/NaN/Inf in 'x'", "no valid set of
coefficients"), so each rung supplies Poisson coefficients as starting
values – the documented remedy – and seeds `init.theta` from a moment
estimate.

## Usage

``` r
.nb_disp_fit_one(week, y, wt, trend_df_per_year = 2, n_harmonics = 2L)
```

## Arguments

- week:

  Integer week index.

- y:

  Numeric weekly totals.

- wt:

  Numeric weekly observation weights.

- trend_df_per_year:

  Spline degrees of freedom per year of data.

- n_harmonics:

  Number of seasonal harmonic pairs.

## Value

A one-row data.frame; see
[`est_nb_dispersion`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_nb_dispersion.md).
