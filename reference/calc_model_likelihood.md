# Compute the total model likelihood (Simplified)

A simplified, robust wrapper for scoring model fits in MOSAIC. The core
is a Negative Binomial (NB) time-series log-likelihood per location and
outcome (cases, deaths) with a weighted MoM dispersion estimate and a
small `k_min` floor.

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
  nb_k_min = 3,
  zero_buffer = TRUE,
  verbose = FALSE,
  add_max_terms = TRUE,
  add_peak_timing = TRUE,
  add_peak_magnitude = TRUE,
  add_cumulative_total = TRUE,
  add_wis = TRUE,
  weight_max_terms = 0.5,
  weight_peak_timing = 0.5,
  weight_peak_magnitude = 0.5,
  weight_cumulative_total = 0.3,
  weight_wis = 0.8,
  sigma_peak_time = 1,
  sigma_peak_log = 0.5,
  penalty_unmatched_peak = -3,
  wis_quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975),
  cumulative_timepoints = c(0.25, 0.5, 0.75, 1),
  enable_guardrails = TRUE,
  floor_likelihood = -999999999,
  guardrail_verbose = FALSE,
  cumulative_over_ratio = 10,
  cumulative_under_ratio = 0.1,
  min_cumulative_for_check = 100,
  negative_correlation_threshold = 0,
  max_timestep_ratio = 100,
  min_timestep_ratio = 0.01,
  min_obs_for_ratio = 1
)
```

## Arguments

- obs_cases, est_cases:

  Matrices `n_locations x n_time_steps` of observed and estimated cases.

- obs_deaths, est_deaths:

  Matrices `n_locations x n_time_steps` of observed and estimated
  deaths.

- weight_cases, weight_deaths:

  Optional scalar weights for case/death blocks. Default 1.

- weights_location:

  Optional length-`n_locations` non-negative weights.

- weights_time:

  Optional length-`n_time_steps` non-negative weights.

- config:

  Optional LASER configuration list containing location_name,
  date_start, and date_stop.

- nb_k_min:

  Numeric; minimum NB dispersion floor used for the core NB likelihood
  and WIS quantiles. Default `3`.

- zero_buffer:

  Kept for backward compatibility (not used by the NB core).

- verbose:

  Logical; if `TRUE`, prints component summaries per location.

- add_max_terms:

  Logical; legacy max Poisson terms. Default `FALSE`.

- add_peak_timing, add_peak_magnitude, add_cumulative_total:

  Logical; default `TRUE`.

- add_wis:

  Logical; default `FALSE`.

- weight_max_terms:

  Component weight for max terms. Default `0.5`.

- weight_peak_timing, weight_peak_magnitude, weight_cumulative_total:

  Component weights. Defaults `0.5, 0.5, 0.3`.

- weight_wis:

  Component weight for optional WIS term.

- sigma_peak_time:

  SD (weeks) for peak timing Normal; default `1`.

- sigma_peak_log:

  Base SD on log-scale for peak magnitude; default `0.5`. Automatically
  scaled by sqrt(100/max(peak_obs,100)) to allow more variance for
  smaller peaks.

- penalty_unmatched_peak:

  Log-likelihood penalty for unmatched peaks; default `-3`.

- wis_quantiles:

  Quantiles for WIS if enabled.

- cumulative_timepoints:

  Fractions for cumulative progression; default `c(0.25,0.5,0.75,1)`.

- enable_guardrails:

  Logical; default `TRUE`.

- floor_likelihood:

  Numeric; hard floor returned on violations. Default `-999999999`.

- guardrail_verbose:

  Logical; print guardrail reasons.

- cumulative_over_ratio, cumulative_under_ratio:

  Cumulative ratio bounds (default `10`, `0.1`).

- min_cumulative_for_check:

  Minimum observed total to apply ratio checks; default `100`.

- negative_correlation_threshold:

  Correlation floor; default `0` (requires positive correlation).

- max_timestep_ratio:

  Maximum ratio of estimated to observed per timestep (default `100`).

- min_timestep_ratio:

  Minimum ratio of estimated to observed per timestep (default `0.01`).

- min_obs_for_ratio:

  Minimum observed value to apply ratio checks (default `1`).

## Value

Scalar total log-likelihood (finite) or `floor_likelihood` if floored.
May be `NA_real_` if all locations contribute nothing.

## Details

By default, three "shape" terms are included with modest weights: (1)
multi-peak timing (Normal on time differences for matched peaks), (2)
multi-peak magnitude (log-Normal on ratios with adaptive sigma), and (3)
cumulative progression (NB at a few cumulative fractions).

Minimal inline guardrails floor the score on egregious fits (cumulative
over/under prediction, per-timestep caps, and negative correlation).
Optional max terms and WIS are kept but OFF by default.
