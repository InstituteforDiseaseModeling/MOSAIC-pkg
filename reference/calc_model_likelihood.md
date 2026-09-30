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

- verbose:

  If `TRUE`, prints component summaries per location.

- weight_peak_timing, weight_peak_magnitude:

  Weights for peak terms (T-normalized). Default `0` (OFF). Set \> 0 to
  enable; 0.25 = 25 percent of NB core influence.

- weight_cumulative_total:

  Weight for cumulative progression (T-normalized). Default `0` (OFF).
  Cumulative helper is /end_idx normalized so weights are on the same
  scale as other shape terms.

- weight_wis:

  Weight for WIS term (T-normalized). Default `0` (OFF). Ablation tests
  show 0.10 provides trajectory-shape regularization.

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
`NA_real_` if all locations contribute nothing.

## Details

Optional shape terms are enabled by setting their weight \> 0: peak
timing (Normal), peak magnitude (log-Normal with adaptive sigma),
cumulative progression (NB at cumulative fractions), and Weighted
Interval Score (WIS). All weights default to 0 (OFF).

Shape terms are internally T-normalized so that weight parameters share
a common scale: `weight = 0.25` means the term contributes roughly 25
percent as much as the NB core. Peaks are scaled by `T / N_peaks`,
cumulative and WIS by `T` (both return per-evaluation averages).

Non-finite per-location LL values are replaced with `-Inf` (zero
importance weight). The NB likelihood naturally produces very negative
scores for bad fits without needing artificial guardrails.
