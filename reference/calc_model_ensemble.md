# Compute Weighted Ensemble Predictions from Multiple Parameter Sets

Runs simulations for multiple parameter sets (with stochastic reruns per
set) and aggregates results using importance weights. Returns a
`mosaic_ensemble` object containing weighted mean, median, and quantile
envelopes for cases and deaths.

With an `observation_model` (as
[`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
supplies), every member trajectory also receives an observation-level
draw consistent with the calibration likelihood, so the interval
envelope is a posterior predictive interval for the OBSERVED counts
rather than for the engine's trajectories alone. Cases: each weekly
total (the blocks the dispersion was estimated on) is drawn from a
negative binomial around the member's weekly total with the run's
per-location weekly size \\k\\, then apportioned to the week's days in
proportion to the member's daily counts (integer counts, every day
within one of its exact share and equal to it in expectation, weekly
totals exact). Deaths (when `deaths_integration` is supplied): each
weekly total gets the integrated deaths likelihood's quasi-Poisson
variance \\\phi_j \mu\\ around the member's expected reported deaths,
coupled to the member's post-hoc deaths so that \\\phi_j = 1\\ leaves
them unchanged. The draws are seeded from each member's simulation seed.
The central lines (`*_mean`, `*_median`) stay ENGINE-level: the noise is
mean-preserving, so the engine mean is the exact predictive mean, and
the median is the central trajectory rather than a quantile of the
noise. See `.mosaic_ensemble_summaries()`.

This is the computation half of the ensemble workflow. Use
[`plot_model_ensemble`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_model_ensemble.md)
to render plots from the returned object.

## Usage

``` r
calc_model_ensemble(
  config,
  parameter_seeds = NULL,
  configs = NULL,
  parameter_weights = NULL,
  n_simulations_per_config = 10L,
  envelope_quantiles = c(0.025, 0.25, 0.75, 0.975),
  PATHS = NULL,
  priors = NULL,
  sampling_args = list(),
  n_cases_warmup_mask = 2L,
  mask_final_deaths_step = FALSE,
  score_idx_cases = 1L,
  score_idx_deaths = 1L,
  parallel = FALSE,
  n_cores = NULL,
  root_dir = NULL,
  capture_trajectories = FALSE,
  trajectory_channels = .MOSAIC_TRAJECTORY_CHANNELS_DEFAULT,
  trajectory_n_lines = 150L,
  trajectory_scratch_dir = NULL,
  reduce_trajectories = TRUE,
  deaths_integration = NULL,
  observation_model = NULL,
  verbose = TRUE
)
```

## Arguments

- config:

  Base configuration object (provides observed data and template).

- parameter_seeds:

  Numeric vector of seeds for
  [`sample_parameters`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/sample_parameters.md).
  Each seed generates a different parameter set.

- configs:

  List of pre-sampled configuration objects (direct mode). Mutually
  exclusive with `parameter_seeds`.

- parameter_weights:

  Numeric vector of importance weights, same length as `parameter_seeds`
  or `configs`. Normalized internally to sum to 1. If `NULL`, all
  parameter sets are weighted equally.

- n_simulations_per_config:

  Integer. Stochastic reruns per parameter set. Default `10L`.

- envelope_quantiles:

  Numeric vector of quantiles for confidence intervals. Must be even
  length to form lower/upper pairs. Default
  `c(0.025, 0.25, 0.75, 0.975)` for 50 and 95 percent CIs.

- PATHS:

  List of paths from
  [`get_paths`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_paths.md).
  Required for sampling mode.

- priors:

  Priors object for parameter sampling. Required for sampling mode.

- sampling_args:

  Named list of additional arguments for
  [`sample_parameters`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/sample_parameters.md).

- n_cases_warmup_mask:

  Integer. Number of LEADING cases timesteps that are an
  initial-condition warm-up transient (seeded E flushing into
  new_symptomatic before the SEIR dynamics settle). Default `2L`,
  matching
  [`plot_model_ensemble`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_model_ensemble.md).
  This value is NOT applied to any of the returned series here; it is
  recorded in the returned `artifact_mask` element so downstream scoring
  (R2/bias) can exclude these positions. Set to `0L` to record "no cases
  warm-up mask".

- mask_final_deaths_step:

  Logical. If `TRUE`, record that the FINAL deaths timestep is a
  structural zero to exclude from scoring. That was true of the
  laser-cholera engine (`reported_deaths` written at tick and
  leading-trimmed, so the last slot was never written; laser-cholera
  issue \#82). Since v0.96.0 the R engine reports deaths on the same row
  as cases, and the post-hoc death redraw fills the final column, so the
  default is `FALSE`. This value is NOT applied to any returned series
  here; it is recorded in the returned `artifact_mask` element for
  downstream scoring.

- score_idx_cases, score_idx_deaths:

  Integer (1-based). Per-channel scored time-window START index (burn-in
  / deaths-era start). Columns strictly BEFORE these indices are
  unscored and recorded in `artifact_mask` so R2/bias scoring drops
  them. Default `1L` (no-op). NOT applied to the returned series here.

- parallel:

  Logical. Use parallel cluster for simulations. Default `FALSE`.

- n_cores:

  Integer or `NULL`. Number of cores when `parallel = TRUE`.

- root_dir:

  Character. MOSAIC root directory. Required when `parallel = TRUE`.

- capture_trajectories:

  Logical. When `TRUE`, harvest the comprehensive internal-state
  channels (`trajectory_channels`) from each member and attach a compact
  `$trajectories` (`mosaic_trajectories`) object – a per-channel central
  line (field `$median`, kept for schema stability) + a uniform-thinned
  set of actual member trajectories + derived series.
  `reported_cases`/`reported_deaths` are the ENGINE-level member
  trajectories (`cases_engine_array`/`deaths_engine_array`). Their
  central line follows the trajectory reducer's default `central_method`
  – the weighted median for `reported_cases`, the weighted mean for
  `reported_deaths` and `disease_deaths` – matching the prediction
  plots' `predicted_central`; every other captured channel uses the
  weighted median. `I_total` is the sum of the Isym and Iasym medians,
  `mass_balance` a ratio of compartment weighted means, `CFR` a ratio of
  28-day rolling weighted-mean deaths and cases, and `epidemic_frac` the
  weighted mean of the reconstructed epidemic flag. Default `FALSE`
  ([`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
  enables it for the posterior ensemble; never for the medoid).
  RAM/payload is linear in `length(trajectory_channels)`.

- trajectory_channels:

  Character vector of `model$results` channels to capture when
  `capture_trajectories = TRUE`. Default
  `.MOSAIC_TRAJECTORY_CHANNELS_DEFAULT` (the comprehensive set). The
  documented RAM lever – shorten it to reduce capture cost.

- trajectory_n_lines:

  Integer. Number of uniform-thinned member trajectories retained per
  location for the spaghetti display. Default 150.

- trajectory_scratch_dir:

  Character or `NULL`. Directory for the per-sim trajectory-channel
  scratch spill (stream-to-disk capture). When `NULL` and capturing, a
  temporary directory is created. When provided, the CALLER owns its
  lifecycle (it is not auto-deleted) – used by
  [`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
  to reduce over the optimized subset post-optimization.

- reduce_trajectories:

  Logical. When `TRUE` (default) the trajectory reduction runs here over
  all members and is attached as `$trajectories`. When `FALSE`, the
  channels are spilled to scratch but NOT reduced; the scratch handle is
  returned in `$trajectory_scratch` so the caller can reduce over a
  final (e.g. optimized) subset without re-simulating.

- deaths_integration:

  Optional run-level setup from
  [`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
  (`control$likelihood$.deaths_integration`). When supplied, each
  member's deaths are redrawn after its simulation from the reported
  CFR's posterior given that member's path – the same integration
  calibration scores with – so predicted deaths, true deaths and
  forecast years carry the calibrated CFR; the ensemble also returns
  `cfr_posterior`. When `NULL` (default) deaths are the engine's, drawn
  at the config's `mu_jt` – for configs sampled from the priors that is
  the PRIOR reported CFR, not the calibrated one. To reproduce a run's
  calibrated deaths post hoc, pass
  `readRDS("<dir_output>/2_calibration/deaths_integration.rds")`.

- observation_model:

  Optional observation model for the posterior predictive: a list with
  `k_cases` (weekly negative binomial size per location, or one for all;
  `Inf` = Poisson) and optionally `week_offset` (0-6 days after Monday
  on which each location's reporting weeks start; default 0), or the
  table of `<dir_output>/2_calibration/diagnostics/nb_dispersion.csv`
  (its cases rows, matched by location).
  [`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
  supplies the dispersion its likelihood scored with. The cases draw is
  this weekly negative binomial under either cases scoring rule. Under
  the default `control$likelihood$cases_scoring = "daily"`, whose
  per-day cells at the same \\k\\ imply a weekly variance of about \\C +
  C^2/(7k)\\ for a weekly total \\C\\ (the predictive uses \\C +
  C^2/k\\), the intervals are therefore wider than the likelihood
  implies; under `"weekly"` they match it. The deaths dispersion is read
  from `deaths_integration`. When `NULL` (default) no observation noise
  is drawn and `cases_array`/`deaths_array` are the engine draws.

- verbose:

  Logical. Print progress messages. Default `TRUE`.

## Value

S3 object of class `"mosaic_ensemble"` containing:

- cases_mean:

  Matrix (n_locations x n_time_points) of weighted mean cases, from the
  engine draws (the exact predictive mean).

- cases_median:

  Matrix of weighted median cases: the median engine trajectory.

- deaths_mean:

  Matrix of weighted mean deaths (engine draws).

- deaths_median:

  Matrix of weighted median deaths (engine draws).

- ci_bounds:

  List with `$cases` and `$deaths`, each a list of interval pairs with
  `$lower` and `$upper` matrices: quantiles of the observation-level
  draws (of the engine draws when no `observation_model` is given).

- predictive_median:

  List with `$cases` and `$deaths`: the 0.5 quantile of the same draws
  as `ci_bounds`, the median a proper interval score (WIS) pairs with
  those intervals, and the `predicted_median` of the prediction CSVs
  [`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
  writes. Equal to `*_median` when no observation noise was drawn.

- obs_cases:

  Observed cases matrix from config.

- obs_deaths:

  Observed deaths matrix from config.

- cases_array:

  4-D array (n_locations x n_time_points x n_param_sets x n_stoch) of
  OBSERVATION-level draws (the engine draws when no `observation_model`
  is given).

- deaths_array:

  4-D array of OBSERVATION-level deaths when the deaths received
  quasi-Poisson overdispersion (an `observation_model` and a
  `deaths_integration` with some \\\phi_j \> 1\\); otherwise the engine
  deaths, identical to `deaths_engine_array` (no `deaths_integration`,
  every \\\phi_j \le 1\\, or no `observation_model`).

- cases_engine_array:

  4-D array of the ENGINE-level reported cases (the member trajectories
  before observation noise), same dimensions. The medoid, R_eff,
  trajectory and implied-CFR consumers read these.

- deaths_engine_array:

  4-D array of engine-level reported deaths (after the post-hoc CFR
  redraw when `deaths_integration` is supplied).

- observation_model:

  List recording the observation noise applied: `cases` (logical,
  whether cases received NB noise), `deaths` (logical, whether deaths
  received quasi-Poisson overdispersion: only where some
  `phi_deaths > 1`; at \\\phi = 1\\ the engine deaths, binomial draws
  around the expected deaths, already are the observation-level deaths),
  `k_cases`, `week_offset` and `phi_deaths` (per location, `NULL` when
  not applied).

- parameter_weights:

  Normalized weight vector.

- seeds:

  Integer vector of per-member simulation seeds, aligned with the
  parameter dimension of the arrays (member `i` \<-\> `seeds[i]`). Bound
  to the parameter set that produced each member so consumers (e.g.
  medoid selection) need not rely on positional alignment with an
  external vector.

- n_param_sets:

  Number of parameter sets.

- n_simulations_per_config:

  Stochastic runs per parameter set.

- n_successful:

  Number of successful simulations.

- location_names:

  Character vector of location names.

- n_locations:

  Number of locations (rows of the central matrices).

- n_time_points:

  Number of time steps (columns of the central matrices).

- date_start:

  Simulation start date.

- date_stop:

  Simulation end date.

- envelope_quantiles:

  Quantiles used for CI envelopes.

- spatial_hazard_ensemble:

  Element-wise median over successful members of the engine's
  `spatial_hazard` (locations x time), or `NULL`.

- coupling_ensemble:

  Element-wise median of the engine's `coupling` matrix (locations x
  locations), or `NULL`.

- pi_ij_ensemble:

  Element-wise median of the engine's `pi_ij` mobility matrix (locations
  x locations), or `NULL`.

- trajectories:

  `mosaic_trajectories` object when `capture_trajectories = TRUE` and
  `reduce_trajectories = TRUE` (see `capture_trajectories`), else
  `NULL`.

- trajectory_scratch:

  Scratch-spill handle when `capture_trajectories = TRUE` and
  `reduce_trajectories = FALSE`, for a caller-side reduce over a final
  subset; else `NULL`.

- artifact_mask:

  List recording the engine-artifact masking spec for downstream
  scoring: `$cases_warmup` (integer, leading cases timesteps to
  exclude), `$deaths_final` (logical, exclude the final deaths
  timestep), and `$score_idx_cases`/`$score_idx_deaths` (integer,
  1-based per-channel scored-window start; columns before are dropped).
  The central/quantile/array fields above are RAW (unmasked); this spec
  is the contract scoring sites use to drop artifact positions.

- cfr_posterior:

  When `deaths_integration` is supplied: a data frame with one row per
  location and calendar year – `location`, `year`, `cfr_median`,
  `cfr_lower`, `cfr_upper` (the weighted median and 95% interval over
  members of each member's mean daily reported CFR in that year) and
  `prior_cfr` (the prior `mu_jt`'s mean over the same days). Conditional
  on each member's modelled cases. `NULL` otherwise.

- forecast_shift:

  When `deaths_integration` is supplied: one value per location, the
  weighted mean over members of the posterior-mode logit CFR deviation
  for the location's latest observed year (`NA` for a location with no
  forecast years).
  [`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
  centres forecast years on it. `NULL` otherwise.

## See also

[`plot_model_ensemble`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_model_ensemble.md)
to render plots from this object.
