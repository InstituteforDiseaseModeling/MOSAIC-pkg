# Estimate negative-binomial dispersion from surveillance observations

Estimates the conditional NB dispersion `k` for each location, at the
data's native weekly reporting resolution and honouring per-observation
confidence weights. The mean is modelled with a spline trend plus
seasonal harmonics and the dispersion estimated by maximum likelihood
([`MASS::glm.nb`](https://rdrr.io/pkg/MASS/man/glm.nb.html)), following
the Farrington/Noufaily convention.

## Usage

``` r
est_nb_dispersion(
  obs,
  weights_obs = NULL,
  date_start,
  location_name = NULL,
  shrink = TRUE,
  trend_df_per_year = 2,
  n_harmonics = 2L,
  verbose = FALSE
)
```

## Arguments

- obs:

  Numeric matrix of observations, `n_locations x n_time_steps`, on a
  daily grid.

- weights_obs:

  Optional numeric matrix of per-observation confidence weights with the
  same dimensions as `obs`.

- date_start:

  Start date of the observation grid (`Date` or a string coercible to
  one).

- location_name:

  Optional character vector of location identifiers used to label the
  result.

- shrink:

  Logical; apply empirical-Bayes shrinkage toward the mean-dispersion
  trend across locations. Default `TRUE`.

- trend_df_per_year:

  Spline degrees of freedom per year for the mean model. Default `2`.

- n_harmonics:

  Number of seasonal harmonic pairs. Default `2`.

- verbose:

  Logical; report a summary of the fit. Default `FALSE`.

## Value

A data.frame with one row per location: `location`, `n_weeks`,
`mean_weekly`, `weekly_share` and `week_offset` (detected reporting
cadence), `k_raw`, `se`, `trend_df`, `rung`, `identified`, `status`, and
`k` (the shrunk value actually used).

## Details

The returned `k` is on R's `dnbinom(mu=, size=)` scale and is used
directly by
[`calc_model_likelihood`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_likelihood.md).
`k = Inf` denotes the Poisson limit and is a valid, intended result.

## Examples

``` r
if (FALSE) { # \dontrun{
cfg <- MOSAIC::config_default
est_nb_dispersion(cfg$reported_cases, cfg$reported_cases_weight,
                  date_start = cfg$date_start,
                  location_name = cfg$location_name, verbose = TRUE)
} # }
```
