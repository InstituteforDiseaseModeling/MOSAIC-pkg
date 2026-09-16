# Run MOSAIC Calibration Workflow

**Complete Bayesian calibration workflow with full control over model
specification.**

This function accepts pre-configured config and priors objects,
allowing:

- Completely custom configs (non-standard locations, custom data)

- Fine-grained control over all model parameters

- Testing with synthetic configurations

Executes the full MOSAIC calibration workflow:

1.  Adaptive calibration with R-squared convergence detection

2.  Predictive batches with model-based sizing and ESS re-evaluation

3.  Post-hoc subset optimization

4.  Posterior quantile and distribution estimation

5.  Posterior predictive checks and uncertainty quantification

## Usage

``` r
run_MOSAIC(
  config,
  priors,
  dir_output,
  control = NULL,
  resume = FALSE,
  cluster = NULL,
  ...
)

run_mosaic(
  config,
  priors,
  dir_output,
  control = NULL,
  resume = FALSE,
  cluster = NULL,
  ...
)
```

## Arguments

- config:

  Named list of simulation configuration (REQUIRED). Contains
  location_name, reported_cases, reported_deaths, and all model
  parameters. Create with custom data or obtain via
  [`get_location_config()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_location_config.md).

- priors:

  Named list of prior distributions (REQUIRED). Contains distribution
  specifications for all parameters. Create custom or obtain via
  [`get_location_priors()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_location_priors.md).

- dir_output:

  Character. Output directory for this calibration run (REQUIRED). All
  results will be saved here. Must be unique per run.

- control:

  Control list created with
  [`mosaic_control_defaults()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/mosaic_control_defaults.md).
  If `NULL`, uses defaults. Controls calibration strategy, parameter
  sampling, parallelization, and output settings. Key settings:

  - `calibration$n_simulations`: NULL for auto mode, integer for fixed
    mode

  - `calibration$n_iterations`: stochastic engine iterations per
    parameter set (default: 3)

  - `calibration$max_simulations_total`: Maximum total simulations
    (default: 100000)

  - `sampling`: Which parameters to sample vs hold fixed

  - `parallel`: Cluster settings for parallel execution

- resume:

  Logical (default `FALSE`). When `TRUE`, the run reconstructs
  calibration state from the per-sim shards already present in
  `<dir_output>/2_calibration/samples/` and continues instead of
  starting fresh. The shards on disk are the sole source of truth (an
  internal `resume_checkpoint.rds` restores the adaptive ESS/phase state
  when present, and is removed when the run completes; otherwise the
  state is reconstructed from the shards). Behaviour by mode: in
  adaptive (auto) mode the run continues from `max(sim_id)+1` and does
  *not* backfill interior gaps (a lost/quarantined shard reduces the
  pool); in fixed mode any missing id within the target is re-run so the
  exact target is met.

  Resume is **rejected** (hard error) when:

  - `control$paths$clean_output = TRUE` (the wipe would delete the
    shards);

  - the run already completed – a consolidated
    `2_calibration/samples.parquet` exists that the on-disk shards would
    shrink (start fresh, or remove it to recompute);

  - the supplied `config`, `priors`, `control$likelihood`,
    `control$sampling`, `control$calibration$n_iterations`, or the
    calibration mode (auto vs fixed) differ from those persisted in
    `1_inputs/` (each changes the draws or likelihood, making the pool
    incomparable);

  - the run directory was created before MOSAIC v0.68.0, so its shards
    came from the Python `laser-cholera` engine rather than the R one
    (the two agree statistically but not draw-for-draw, so pooling them
    would give a posterior from neither simulator);

  - the likelihood-value provenance differs – i.e. the existing shards
    were scored by a different likelihood engine or implementation than
    the current session would produce (an archived Python-scored shard,
    or an R likelihood-code change that altered values).

  If the originating MOSAIC version cannot be determined (no
  `1_inputs/environment.json`, or an unparseable version) the engine
  check is skipped with a warning. Has no effect when no shards exist
  (equivalent to a fresh run).

- cluster:

  Optional pre-built R parallel cluster. When provided, skips cluster
  creation and teardown, reusing existing workers. Useful for staged
  estimation where multiple `run_MOSAIC` calls share a cluster. The
  caller retains ownership of any cluster passed and is responsible for
  stopping it after `run_MOSAIC` returns.

- ...:

  Reserved. Supplying a removed argument (`dask_spec`) or any
  unrecognised argument raises an error naming it rather than silently
  ignoring it. See
  [removed_api](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/removed_api.md).

## Value

Invisibly returns a list with:

- dirs:

  Named list of output directories

- files:

  Named list of key output files

- summary:

  Named list with run statistics (batches, sims, converged, runtime)

## Control Structure

See
[`mosaic_control_defaults()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/mosaic_control_defaults.md)
for complete documentation. The control structure contains:

- calibration:

  n_simulations, n_iterations, max_simulations_total,
  batch_size_adaptive, etc.

- sampling:

  sample_tau_i, sample_mobility_gamma, sample_mu_j, etc.

- parallel:

  enable, n_cores, type, progress

- paths:

  clean_output, plots

- targets:

  ESS_param, ESS_best, A_best, CVw_best, etc.

- io:

  format, compression, compression_level

## Output Files

Results are organized in a structured directory tree:

- `1_inputs/`: Configuration files (JSON format)

- `2_calibration/samples/`: Simulation results (Parquet format)

- `2_calibration/diagnostics/`: ESS metrics, convergence results

- `2_calibration/posterior/`: Posterior quantiles and distributions

- `3_results/figures/`: Diagnostic, parameter, and prediction plots

- `3_results/`: Final combined results

## See also

[`mosaic_control_defaults()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/mosaic_control_defaults.md)
for building control structures

## Examples

``` r
if (FALSE) { # \dontrun{
# === BASIC CUSTOM CONFIG ===

# Load and modify default config
config <- get_location_config(iso = "ETH")
config$population_size <- 1000000

priors <- get_location_priors(iso = "ETH")

run_MOSAIC(
  config = config,
  priors = priors,
  dir_output = "./output"
)

# === MULTI-LOCATION WITH CUSTOM CONTROL ===

config <- get_location_config(iso = c("ETH", "KEN", "TZA"))
priors <- get_location_priors(iso = c("ETH", "KEN", "TZA"))

ctrl <- mosaic_control_defaults(
  calibration = list(
    n_simulations = 5000,  # Fixed mode
    n_iterations = 5
  ),
  parallel = list(enable = TRUE, n_cores = 16)
)

run_MOSAIC(config, priors, "./output", ctrl)

# === CUSTOM PRIORS FOR SENSITIVITY ===

config <- get_location_config(iso = "ETH")
priors <- get_location_priors(iso = "ETH")

# Tighten transmission rate prior
priors$tau_i$shape <- 20
priors$tau_i$rate <- 4

run_MOSAIC(config, priors, "./output")

# === COMPLETELY CUSTOM CONFIG ===

# Non-standard location names
custom_config <- list(
  location_name = c("Region1", "Region2"),
  reported_cases = my_cases_data,
  reported_deaths = my_deaths_data,
  # ... all other simulation parameters
)

custom_priors <- list(
  # ... custom prior specifications
)

run_MOSAIC(custom_config, custom_priors, "./output")

# === RESUME AN INTERRUPTED RUN ===

# If a calibration is killed mid-run, re-call with resume = TRUE and the same
# config/priors/output directory. Completed simulations on disk are reused and
# the run continues from the next sim_id (bit-identical to an uninterrupted run).
run_MOSAIC(config, priors, "./output", resume = TRUE)
} # }
```
