# Deaths log-likelihood with the reported CFR integrated out

Scores observed deaths against a simulated path with the reported case
fatality ratio (CFR) integrated out analytically. Given a path, expected
reported deaths on each day are the CFR times a known exposure, so the
CFR's level can be solved for per path rather than sampled. The CFR is
modelled as the time-varying prior `mu_jt` shifted on the logit scale by
a location-level offset and one level per calendar year:
\$\$\mathrm{logit}\\\mu\_{jt} = \mathrm{logit}\\\mu^{0}\_{jt} + a_j +
\sum_y B_y(t)\\\delta\_{j,y},\qquad a_j \sim N(0, s_j^2),\quad
\delta\_{j,y} \sim N(0, \sigma_y^2)\\ (y \le y^\*\_j),\quad
\delta\_{j,y} \sim N(f_j, \sigma_y^2)\\ (y \> y^\*\_j),\$\$ where
\\B_y(t)\\ is 1 inside calendar year \\y\\ and blends linearly into the
next year over the 60 days centred on each 1 January, so the CFR has no
step at a year boundary. Each year's deviation is a LEVEL for that year:
a year observed only in part is fitted to the months observed and
applied unchanged to the rest of the year (an interpolated basis would
extrapolate the within-year trend past the data instead). \\y^\*\_j\\ is
the latest year observed past its New Year blend (the year of the last
scored day minus 30 days), and each later year is a forecast year
centred on the forecast shift \\f_j\\ with the usual year-to-year spread
\\\sigma_y\\.
[`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
sets \\f_j\\ to the posterior ensemble's mean deviation for \\y^\*\_j\\,
so a forecast continues the latest calibrated CFR shift shared by the
members – without each member's own case error, which its own
\\\delta\_{j,y^\*\_j}\\ also absorbs – instead of reverting to the
prior's long-run level.

Deaths are aggregated to reporting weeks and scored with a quasi-Poisson
likelihood: the Poisson log-likelihood divided by a per-location
dispersion \\\phi_j\\, with a small additive background on each week's
expected deaths. The quasi-Poisson score for the CFR level is the
Poisson score, so given the path the fitted CFR reproduces the observed
deaths totals (a negative binomial score would weight low-count weeks
far above the peak and bias the level); \\\phi_j\\ tempers the
likelihood for deaths that scatter more than Poisson. For each location
the offsets are fitted by Newton's method and the marginal likelihood is
the Laplace approximation at the mode.

## Usage

``` r
calc_log_likelihood_deaths_integrated(
  obs_deaths,
  exposure,
  base_logit,
  dates,
  sd_shift,
  sd_year,
  onset_dates = dates,
  dispersion = 1,
  background_rel = 0.02,
  weights = NULL,
  week_offset = NULL,
  years = NULL,
  forecast_shift = 0
)
```

## Arguments

- obs_deaths:

  Matrix \[locations x days\] (or vector, one location) of observed
  reported deaths.

- exposure:

  Matrix of the same shape: expected reported deaths per unit reported
  CFR on each day, `(rho / chi_epidemic) * onsets` at the onset day.
  Must be finite and non-negative.

- base_logit:

  Matrix of the same shape: logit of the prior `mu_jt` at the onset day.

- dates:

  Date vector, one per day: the reporting day (defines the weekly
  blocks).

- sd_shift:

  Numeric, length 1 or one per location: prior SD of the location offset
  \\a_j\\ (logit scale).

- sd_year:

  Numeric scalar: prior SD of each year deviation \\\delta\_{j,y}\\
  (logit scale).

- onset_dates:

  Date vector, one per day: the onset day of the deaths reported that
  day, which positions the year deviations (default `dates`).

- dispersion:

  Numeric, length 1 or one per location: the quasi-Poisson dispersion
  \\\phi_j \> 0\\ (1 = Poisson).

- background_rel:

  Numeric scalar \>= 0: the additive background on each week's expected
  deaths, as a fraction of the location's mean scored weekly deaths
  (floored at 1e-4).

- weights:

  Optional matrix of per-day scoring weights (0 or `NA` = not scored);
  defaults to 1 wherever `obs_deaths` is finite. Used as given (not
  renormalised).

- week_offset:

  Optional integer, length 1 or one per location, 0-6 days from Monday
  for the reporting-week boundary; detected from `obs_deaths` when
  `NULL`.

- years:

  Optional integer vector of the years that carry deviations (default:
  every year of `onset_dates`); years after the latest observed year are
  centred on `forecast_shift`, and other years without scored weeks keep
  their prior. Days before the first year or after the last take the
  nearest year's level.

- forecast_shift:

  Numeric, length 1 or one per location: the logit-scale deviation
  \\f_j\\ each forecast year is centred on (default 0: forecast years
  revert to the prior level).

## Value

A list with `ll` (marginal log-likelihood per location), `theta` (matrix
\[locations x (1 + years)\]: the offset `a` and the year deviations at
the mode), `theta_sd` (their Laplace posterior SDs), `vcov` (list of
Laplace covariance matrices), `converged` (logical per location),
`n_weeks` (scored weeks per location), `years`, `dispersion` and
`background` (per location).

## Details

A reporting week is scored when every one of its days inside the data is
finite and carries positive weight, so a week cut by the start or end of
the data is scored on the days it has. Its weight is the mean weight of
its days. The background is the same relative floor the cases channel
applies to a cell whose prediction is zero, so a week with no onsets but
observed deaths costs a bounded, data-scaled amount rather than an
arbitrary constant.

A forecast year that no scored day touches leaves the marginal
likelihood exactly as it is with `forecast_shift = 0`; only its
posterior moves, to the shift. Data that end less than 30 days after a 1
January reach the new year only through the blend, so the year before
stays \\y^\*\_j\\ and the new year is a forecast year, centred on the
shift and still fitted to those days.

## See also

[`calc_model_likelihood`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_likelihood.md),
[`est_CFR_hierarchical`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_CFR_hierarchical.md)

## Examples

``` r
dates <- seq(as.Date("2024-01-01"), by = "day", length.out = 140)
set.seed(1)
expo <- rpois(140, 400) * 0.423 / 0.75
obs <- rpois(140, 0.02 * expo)
fit <- calc_log_likelihood_deaths_integrated(
     obs, expo, base_logit = rep(qlogis(0.015), 140), dates = dates,
     sd_shift = 0.5, sd_year = 0.7)
plogis(qlogis(0.015) + sum(fit$theta[1, ]))   # recovered reported CFR, ~0.02
#> [1] 0.01983853
```
