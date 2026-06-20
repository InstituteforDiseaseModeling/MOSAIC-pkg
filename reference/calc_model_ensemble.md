# Compute Weighted Ensemble Predictions from Multiple Parameter Sets

Runs LASER simulations for multiple parameter sets (with stochastic
reruns per set) and aggregates results using importance weights. Returns
a `mosaic_ensemble` object containing weighted mean, median, and
quantile envelopes for cases and deaths.

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
  mask_final_deaths_step = TRUE,
  parallel = FALSE,
  n_cores = NULL,
  root_dir = NULL,
  precomputed_results = NULL,
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

  Integer. Stochastic LASER reruns per parameter set. Default `10L`.

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

  Logical. If `TRUE` (default, matching
  [`plot_model_ensemble`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_model_ensemble.md)),
  record that the FINAL deaths timestep is a laser-cholera structural
  zero (`reported_deaths` written at tick then leading-trimmed, so the
  last slot is never written; laser issue \#82). This value is NOT
  applied to any returned series here; it is recorded in the returned
  `artifact_mask` element for downstream scoring.

- parallel:

  Logical. Use parallel cluster for simulations. Default `FALSE`.

- n_cores:

  Integer or `NULL`. Number of cores when `parallel = TRUE`.

- root_dir:

  Character. MOSAIC root directory. Required when `parallel = TRUE`.

- precomputed_results:

  Optional list of pre-gathered LASER results (e.g. from Dask). Each
  element must have `$param_idx`, `$stoch_idx`, `$reported_cases`,
  `$reported_deaths`, and `$success`.

- verbose:

  Logical. Print progress messages. Default `TRUE`.

## Value

S3 object of class `"mosaic_ensemble"` containing:

- cases_mean:

  Matrix (n_locations x n_time_points) of weighted mean cases.

- cases_median:

  Matrix of weighted median cases.

- deaths_mean:

  Matrix of weighted mean deaths.

- deaths_median:

  Matrix of weighted median deaths.

- ci_bounds:

  List of CI pairs, each with `$lower` and `$upper` matrices.

- obs_cases:

  Observed cases matrix from config.

- obs_deaths:

  Observed deaths matrix from config.

- cases_array:

  4-D array (n_locations x n_time_points x n_param_sets x n_stoch).

- deaths_array:

  4-D array matching cases_array dimensions.

- parameter_weights:

  Normalized weight vector.

- seeds:

  Integer vector of per-member simulation seeds, aligned with the
  parameter dimension of `cases_array` (member i \<-\> seedsi). Bound to
  the parameter set that produced each member so consumers (e.g. medoid
  selection) need not rely on positional alignment with an external
  vector.

- n_param_sets:

  Number of parameter sets.

- n_simulations_per_config:

  Stochastic runs per parameter set.

- n_successful:

  Number of successful simulations.

- location_names:

  Character vector of location names.

- date_start:

  Simulation start date.

- date_stop:

  Simulation end date.

- envelope_quantiles:

  Quantiles used for CI envelopes.

- artifact_mask:

  List recording the engine-artifact masking spec for downstream
  scoring: `$cases_warmup` (integer, leading cases timesteps to exclude)
  and `$deaths_final` (logical, exclude the final deaths timestep). The
  central/quantile/array fields above are RAW (unmasked); this spec is
  the contract scoring sites use to drop artifact positions.

## See also

[`plot_model_ensemble`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_model_ensemble.md)
to render plots from this object.
