# Compute the total model likelihood

Scores model fits against observed data using a Negative Binomial (NB)
time-series log-likelihood per location and outcome (cases, deaths) with
a weighted MoM dispersion estimate and a `k_min` floor.

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
  config = NULL,
  nb_k_min_cases = 3,
  nb_k_min_deaths = 3,
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

- config:

  Optional LASER config list (location_name, date_start, date_stop).

- nb_k_min_cases:

  Minimum NB dispersion floor for cases. Default `3`.

- nb_k_min_deaths:

  Minimum NB dispersion floor for deaths. Default `3`.

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
