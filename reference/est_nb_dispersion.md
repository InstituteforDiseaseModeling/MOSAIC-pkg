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
  verbose = FALSE,
  obs_tier = NULL,
  panel_trend = NULL
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

- obs_tier:

  Optional integer matrix of surveillance trust tiers with the same
  dimensions as `obs` (`config$reported_tier`: 1 observed, 2
  reconstructed, 3 imputed). When supplied, only weeks whose seven days
  are all tier 1 enter the fit: a reconstructed week (a WHO multi-week
  report spread evenly) or an imputed one (a Fourier curve through an
  annual total) has a synthetic shape that reads as low noise and
  inflates `k`. The Poisson rule above still uses every tier. Default
  `NULL` (all weeks).

- panel_trend:

  Optional list with numeric `intercept` and `slope`: a cross-location
  trend `log k = intercept + slope * log(mean weekly count)` taken by a
  location without a usable estimate of its own (no estimate, or a fit
  clamped at the lower bound; see Details).
  [`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
  supplies the cases trend fitted on config_default with
  `burn_in_days = 45` (`MOSAIC:::.NB_DISP_PANEL_TREND`), whatever the
  run's burn-in; refitted on the window of the control default (30) it
  barely moves (BFA, CIV, CMR, ZAF, UGA 0.88, 0.97, 1.63, 1.09, 0.95
  instead of 0.89, 0.98, 1.65, 1.10, 0.96), far inside the trend's
  residual SD of 1.19 on log k. Default `NULL`.

## Value

A data.frame with one row per location: `location`, `weekly_share` and
`week_offset` (detected reporting cadence), `n_weeks` (weeks in the fit;
every scored week when the location takes the Poisson limit for sparse
data, where no fit is attempted), `n_weeks_excluded` (scored weeks left
out of the fit as reconstructed or imputed; 0 when no fit is attempted),
`mean_weekly` (mean weekly count over all scored weeks), `k_raw`, `se`,
`trend_df`, `rung`, `identified`, `status`, `panel_trend` (`TRUE` where
`k` comes from `panel_trend`), and `k` (the value actually used).
[`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
writes this table, for both channels, to
`2_calibration/diagnostics/nb_dispersion.csv`. Where
[`MASS::glm.nb`](https://rdrr.io/pkg/MASS/man/glm.nb.html) fails on a
rung whose Poisson mean converges, `theta` is estimated by
[`MASS::theta.ml`](https://rdrr.io/pkg/MASS/man/theta.md.html) at that
Poisson mean. `status` records this fallback only as
`ok_theta_ml_at_full_df`, which ranks below `clamped_lower_bound` and
`ok_not_identified`: a clamped or unidentified row does not show whether
its `theta` came from the fallback (`rung` and `trend_df` give the mean
model it was estimated at).

## Details

The returned `k` is on R's `dnbinom(mu=, size=)` scale and is used
directly by
[`calc_model_likelihood`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_likelihood.md):
its default per-day cell rule applies it to every day, and
`cases_scoring = "weekly"` scores the cases on the same weekly totals it
was estimated on. `k = Inf` denotes the Poisson limit and is a valid,
intended result.

A location with fewer than 20 weeks, 15 cases or 5 non-zero weeks over
all its scored weeks takes the Poisson limit. Otherwise its own fit can
still fail to give an estimate (status `no_estimate_*`): every rung of
the mean model fails, the standard error of `theta` is not finite, or
the fit collapsed (`theta` ran to the zero boundary, so its SE is below
1e-4 of the dispersion it would report; on config_default this is how
bursty series with reporting dumps defeat the smooth mean), or, with
`obs_tier`, the observed weeks alone fall short of the minimum. Such a
location takes `panel_trend` when it is supplied, evaluated at its mean
weekly count; without it, it borrows the run's own mean-dispersion trend
(five or more estimated locations), the median of the estimated
locations, or, alone, the Poisson limit.

With `panel_trend`, a fit clamped at the lower bound of 0.1 (status
`clamped_lower_bound`) takes the trend too, at every scale. The clamp is
censoring, not a measurement. Where a series' few non-zero observed
weeks are mostly the edges of short outbreaks whose middle weeks are
reconstructed and left out of the fit (UGA on config_default v6.1: 10
non-zero of 84 observed weeks), the smooth mean cannot follow the
outbreaks, their variance stays in the residual, and the fit returns the
bound whatever the true `k`: on synthetic series of that shape it did so
for a Poisson, a `k = 1` and a `k = 5` reporting process alike. At
`k = 0.1` the cases score is several times less sensitive to the level
(on UGA's two observed years a twofold level error costs 3.1 nats under
the daily cases rule and 0.5 under the weekly one, against 22 and 4.6 at
the trend's 0.96), and the weekly observation-level predictive puts at
least half its mass on 0 for any weekly mean up to 102. The row keeps
`status = "clamped_lower_bound"` and `k_raw` (the fit) with
`panel_trend = TRUE` (the `k` used). Without `panel_trend` a clamped fit
keeps the bound, shrunk toward the run's own trend when five or more
locations have an estimate.

## Examples

``` r
if (FALSE) { # \dontrun{
cfg <- MOSAIC::config_default
est_nb_dispersion(cfg$reported_cases, cfg$reported_cases_weight,
                  date_start = cfg$date_start,
                  location_name = cfg$location_name, verbose = TRUE)
} # }
```
