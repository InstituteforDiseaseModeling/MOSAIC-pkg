# Compute the total model likelihood

Scores model fits against observed data using a Negative Binomial (NB)
time-series log-likelihood per location and outcome (cases, deaths) with
a per-location NB dispersion estimated by
[`est_nb_dispersion`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_nb_dispersion.md).

## Usage

``` r
calc_model_likelihood(
  obs_cases,
  est_cases,
  obs_deaths,
  est_deaths,
  weight_cases = NULL,
  weight_deaths = NULL,
  weights_location = NULL,
  weights_time = NULL,
  weights_obs_cases = NULL,
  weights_obs_deaths = NULL,
  config = NULL,
  nb_k_cases = NULL,
  nb_k_deaths = NULL,
  eps_rel_cases = 0.02,
  eps_rel_deaths = 0.25,
  ll_deaths_core = NULL,
  cases_scoring = c("daily", "weekly"),
  week_offset = NULL,
  verbose = FALSE,
  weight_peak_timing = 0,
  weight_peak_magnitude = 0,
  weight_cumulative_total = 0,
  weight_wis = 0,
  sigma_peak_time = 1,
  sigma_peak_log = 0.5,
  wis_quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975),
  cumulative_timepoints = c(0.25, 0.5, 0.75, 1)
)
```

## Arguments

- obs_cases, est_cases:

  Matrices `n_locations x n_time_steps`.

- obs_deaths, est_deaths:

  Matrices `n_locations x n_time_steps`.

- weight_cases, weight_deaths:

  Scalar weights for case/death blocks. Default 1.

- weights_location:

  Length-`n_locations` non-negative weights.

- weights_time:

  Length-`n_time_steps` non-negative weights.

- weights_obs_cases, weights_obs_deaths:

  Optional per-observation confidence-weight matrices
  (`n_locations x n_time_steps`, values in `[0,1]`; `NA` where the
  corresponding cell is `NA`). When supplied, the per-cell weight
  multiplies `weights_time` for the NB cases/deaths term respectively,
  and the resulting per-location weight vector is renormalized to
  preserve the current masked-`weights_time` mass so only the trust
  SHAPE matters (cross-location balance stays with `weights_location`).
  Default `NULL` (no per-cell weighting; the exact unweighted code path
  is used, byte-identical to prior behavior). A row that is all-1 on
  finite-obs cells is also routed through the exact unweighted path.
  Only the NB cases/deaths terms are weighted; shape terms are not (v1).

- config:

  Optional simulation config list (location_name, date_start,
  date_stop).

- nb_k_cases:

  NB dispersion for the cases channel: a scalar applied to every
  location, or a vector with one entry per location. `Inf` selects the
  Poisson limit. When `NULL` (default) the dispersion is estimated from
  `obs_cases` via
  [`est_nb_dispersion`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_nb_dispersion.md);
  in
  [`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
  it is precomputed once and supplied here.

- nb_k_deaths:

  NB dispersion for the deaths channel; see `nb_k_cases`.

- eps_rel_cases, eps_rel_deaths:

  Positive scalars. Within each location the predicted mean is floored
  at `max(1e-4, eps_rel * mean(obs))` before the NB density is
  evaluated, separately per channel. The floor is not cosmetic:
  production scores a SINGLE stochastic realisation, so a low-count
  series is full of cells where the realisation is 0 against a positive
  observation, and the size of the floor is what the likelihood pays for
  such a cell. Too small a floor makes zeros ruinous and the optimum
  moves to a draw that over-predicts the level (a Jensen gap:
  `E_seed[LL(est)]` peaks well above `LL(E_seed[est])`). Cases default
  `0.02`; deaths default `0.25`, sized by sweep so the deaths level at
  the likelihood optimum is unbiased. Cases are far less exposed: 13.6
  percent of scored deaths cells predict zero against a positive
  observation, versus 1.7 percent of cases cells.

- ll_deaths_core:

  Optional numeric vector, one value per location: the deaths
  log-likelihood computed with the reported case fatality ratio
  integrated out (`calc_log_likelihood_deaths_integrated()$ll`). When
  supplied it replaces the negative-binomial deaths core, so
  `eps_rel_deaths` and `nb_k_deaths` are not used for the core;
  [`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
  always supplies it. The level-dependent deaths shape terms (peak
  magnitude, cumulative, WIS) are then dropped, with a warning, because
  `est_deaths` is drawn at the prior CFR; deaths peak timing, which does
  not depend on the level, is kept.

- cases_scoring:

  `"daily"` (default) scores the cases core, and the negative-binomial
  deaths core, one cell per time step at the weekly `k` (the cumulative
  term at size `k * n`): the cell rule of MOSAIC v0.100.1 and earlier.
  `"weekly"` scores them on reporting-week totals (see Description). The
  daily rule is applied at the dispersion supplied or estimated now, so
  it does not reproduce a v0.100.1 score by itself: when
  `nb_k_cases`/`nb_k_deaths` are estimated here, a cases location whose
  fit gives no estimate, or is clamped at the lower bound, takes the
  panel trend (v0.100.1 kept such fits at the 0.1 bound), and a config
  with `reported_tier` restricts the estimate to observed weeks.
  Reproducing a v0.100.1 score needs that run's dispersions
  (`nb_k_cases`, `nb_k_deaths` from its `nb_dispersion.csv`) and a
  config without `reported_tier`. `NULL` means `"daily"`.

- week_offset:

  Reporting-week boundary of each location, 0-6 days after Monday
  (length 1 or one per location; `NA` = detect), as returned by
  [`est_nb_dispersion()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_nb_dispersion.md).
  [`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
  supplies the boundaries its dispersion estimate detected, so both use
  the same weeks. `NULL` (default) detects them from `obs_cases` on
  every call, which costs tens of milliseconds per location when
  `nb_k_cases` is supplied: a caller that scores many simulations
  against the same observations should pass
  `est_nb_dispersion()$week_offset`, as
  [`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
  does. Used by the weekly cores only.

- verbose:

  If `TRUE`, prints component summaries per location.

- weight_peak_timing, weight_peak_magnitude:

  Weights for peak terms, scaled by `N_obs / N_peaks`. Default `0`
  (OFF); set \> 0 to enable.

- weight_cumulative_total:

  Weight for cumulative progression, scaled by
  `N_obs / length(cumulative_timepoints)`. Default `0` (OFF).

- weight_wis:

  Weight for the negated WIS term, scaled by
  `N_obs / length(wis_quantiles)`. Default `0` (OFF).

- sigma_peak_time:

  SD (weeks) for peak timing Normal; default `1`.

- sigma_peak_log:

  Base SD on log-scale for peak magnitude; default `0.5`.

- wis_quantiles:

  Quantiles for WIS if enabled.

- cumulative_timepoints:

  Fractions for cumulative progression.

## Value

Scalar total log-likelihood (finite), `-Inf` if non-finite, or
`NA_real_` if no location has data to score. A location has none when
neither channel can be scored: a weekly core needs three scored weeks
(weighted: a weight sum of three), a per-time-step core or a shape term
three usable observations (weighted: a weight sum of three), and with
`ll_deaths_core` the deaths channel counts when it has three usable
observations or a non-zero score (a score of exactly 0 has no scored
week).

## Details

By default (`cases_scoring = "daily"`) the cases are scored one NB cell
per time step at the weekly `k`, the rule of v0.100.1 and earlier.
`cases_scoring = "weekly"` scores them on weekly totals instead. The
surveillance series are weekly totals spread over the days of each
reporting week, and the dispersion is estimated on weekly totals, so the
weekly rule sums the observed and simulated daily cases over the
reporting weeks
[`est_nb_dispersion()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_nb_dispersion.md)
uses (the same block boundaries, from `week_offset`) and scores each
week as one NB cell at the weekly `k`. Scoring every day of a spread
week at the weekly `k`, as the daily rule does, counts its level
information about `7 (k + M) / (7 k + M)` times (`M` the weekly mean; a
median 5.4 over the v0.100.1 national runs, exactly 1 in the Poisson
limit) and ranks draws largely by their within-week noise against a flat
spread. The daily rule is the default all the same: in the v0.101.0
likelihood gate (four countries calibrated under each rule with the same
data, dispersions, intervals and seeds) the weekly rule fitted the cases
worse (see NEWS).

Under the weekly rule a week is scored only when all seven of its days
lie in the scored window and carry a finite observation, a finite
confidence weight and a positive time weight; a week cut by the start or
end of the window is not a weekly total and is dropped, as in the
dispersion estimate. Each week's weight is the mean of its days'
weights: a confidence weight belongs to the reporting week and is the
same on its seven days, so the week keeps the weight its days had and
the weight keeps its role as an exponent on that week's likelihood. The
weights are then made mass-preserving over the scored weeks, as for the
daily cells. The cases floor `eps_rel_cases` applies to the weekly
prediction relative to the mean weekly observation, and a location needs
three scored weeks (weighted: a weight sum of three) for its cases core
to count. A simulated daily count that is not finite on a day of a
scored week makes the weekly cases core `-Inf`: the path failed, and
dropping the week would remove its penalty. (The daily rule keeps its
earlier treatment: a day whose simulated count is missing is left out of
the score, and a path with no usable day scores `-Inf`.) Without a dated
daily grid (no `config$date_start`), or when the time steps are weeks,
the weekly rule scores each time step as one cell. Without
`ll_deaths_core` the negative-binomial deaths core follows the cases
rule (under the weekly rule, weekly deaths totals on the same reporting
weeks at the weekly `nb_k_deaths`).

Optional shape terms are enabled by setting their weight \> 0: peak
timing (Normal), peak magnitude (log-Normal with adaptive sigma),
cumulative progression (NB at cumulative fractions), and Weighted
Interval Score (WIS). All weights default to 0 (OFF).

Each shape term helper returns a per-evaluation value, which is
multiplied by `N_obs / N_eval`, where `N_obs` is the number of daily
time steps with a finite observation in either channel and `N_eval` is
the number of evaluations of that term: peaks are scaled by
`N_obs / N_peaks`, WIS by `N_obs / length(wis_quantiles)` and the
cumulative term by `N_obs / length(cumulative_timepoints)`. This puts
the peak terms on the per-day scale the NB core had when it scored daily
cells. Because the WIS and cumulative helpers already average over their
quantiles and timepoints, a given weight on those two terms carries less
influence than the same weight on the peak terms (with the defaults, 1/5
and 1/4 of `N_obs` times the per-cell value), and changing the number of
quantiles or timepoints changes their influence. The shape terms have
the same definitions under both cases rules: they read the daily series,
`N_obs` counts daily time steps, and the WIS term uses the weekly `k` on
daily cells. The cumulative term sums the days of each prefix and scores
the sum as negative binomial with size `k * n` when each time step is a
cell (the default daily rule, undated input or weekly time steps) and
`k * n / 7` under `cases_scoring = "weekly"` (`n` scored days make
`n / 7` weekly totals at the weekly `k`). Because the weekly cases core
carries about a fifth of the level information of the daily one (less of
a change where `k` is large; none in the Poisson limit), a given shape
weight weighs several times more against the cases core under the weekly
rule than under the default (a median 4.8 times, range 1.8 to 6.5, on
the v0.100.1 national re-selection pools).

Non-finite per-location LL values are replaced with `-Inf` (zero
importance weight). The NB likelihood naturally produces very negative
scores for bad fits without needing artificial guardrails.
