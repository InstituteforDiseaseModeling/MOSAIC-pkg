# Run MOSAIC Calibration Workflow

High-level entry point that executes the full MOSAIC calibration
workflow:

1.  Adaptive calibration with R² convergence detection

2.  Single predictive batch (calculated from calibration phase)

3.  Adaptive fine-tuning with 5-tier batch sizing

4.  Post-hoc subset optimization for NPE priors

5.  Posterior quantile and distribution estimation

6.  Posterior predictive checks and uncertainty quantification

7.  Optional: Neural Posterior Estimation (NPE) stage

The simulation engine samples parameters once per simulation seed, runs
`n_iter` stochastic iterations, collapses likelihoods via log-mean-exp,
and writes individual parquet files per simulation.

Returns default control settings for `run_mosaic()`

Returns default sampling settings for
[`sample_parameters()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/sample_parameters.md)

Returns pre-configured I/O settings for common use cases

Backward-compatible alias for `run_mosaic()`

## Usage

``` r
run_mosaic(
  iso_code,
  dir_output,
  config = NULL,
  priors = NULL,
  sampling_args = NULL,
  n_iter = 3L,
  n_sims = "auto",
  control = NULL,
  resume = FALSE
)

mosaic_run_defaults()

mosaic_sampling_defaults()

mosaic_io_presets(preset = c("default", "debug", "fast", "archive"))

run_MOSAIC(...)
```

## Arguments

- iso_code:

  Character vector of ISO3 codes (e.g., `c("MOZ","MWI","ZMB","ZWE")`).
  Used to load default config and priors if not provided.

- dir_output:

  Character. Output directory for this calibration run (REQUIRED). All
  results (simulations, diagnostics, plots, posteriors) will be saved
  here. Must be unique per run to avoid overwriting results. The
  directory will be created if it does not exist.

- config:

  Named list of model configuration. If `NULL` (default), loads
  location-specific configuration via `get_location_config(iso_code)`.
  Provide custom config to use different outbreak data or model
  settings.

- priors:

  Named list of prior distributions. If `NULL` (default), loads
  location-specific priors via `get_location_priors(iso_code)`. Provide
  custom priors to modify parameter ranges or distributions.

- sampling_args:

  Named list forwarded to
  [`sample_parameters()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/sample_parameters.md).
  If `NULL` (default), uses `mosaic_sampling_defaults()`. Controls which
  parameters are sampled vs held fixed.

- n_iter:

  Integer. Stochastic iterations per simulation (collapsed post-hoc via
  log-mean-exp). Default 3.

- n_sims:

  Either `"auto"` for adaptive ESS-driven batching (stops at
  `max_simulations` if not converged), or a positive integer for a fixed
  number of simulations (runs exactly that many, ignoring
  `max_simulations`).

- control:

  Named list of algorithm settings. Start from `mosaic_run_defaults()`
  and override fields as needed. See Details for structure.

- resume:

  Logical. If `TRUE`, reuses existing per-simulation files and continues
  from last checkpoint (auto mode) or resumes fixed run.

- preset:

  Character. One of "default", "debug", "fast", or "archive"

## Value

Invisibly returns a list with:

- dirs:

  Named list of output directories

- files:

  Named list of key output files

- summary:

  Named list with run statistics (batches, sims, converged, runtime)

## Control Structure

The `control` argument accepts a nested list with these sections:

- paths:

  Behavior flags: `clean_output`, `plots`

- parallel:

  Cluster settings: `n_cores`, `type`, `progress`

- limits:

  Hard constraints: `max_simulations`, `min_ess_check`

- calibration:

  Phase 1: `batch_size`, `min_batches`, `max_batches`, `target_r2`

- fine_tuning:

  Phase 3: `batch_sizes` list (massive/large/standard/precision/final)

- targets:

  Convergence: `ESS_param`, `ESS_param_prop`, `ESS_best`, `A_best`,
  `CVw_best`, `B_min`, `percentile_max`

- npe:

  Stage 2: `enable`, `weight_strategy`

- io:

  File format: `format`, `compression`, `compression_level`

See `mosaic_run_defaults()` for defaults.

## MOSAIC Root Directory

The MOSAIC project root (containing MOSAIC-pkg/, MOSAIC-data/, etc.) is
auto-detected from `getOption("MOSAIC.root")`. Set once per session:

      options(MOSAIC.root = "/path/to/MOSAIC")

Or add to `~/.Rprofile` for permanent configuration.

## Output Files

Results are organized in a structured directory tree:

- `0_setup/`: Configuration files (simulation_params.json, priors.json,
  config_base.json)

- `1_bfrs/outputs/`: Combined simulations.parquet and individual files
  during run

- `1_bfrs/diagnostics/`: ESS metrics, convergence results, subset
  selection

- `1_bfrs/posterior/`: Posterior quantiles and distributions

- `1_bfrs/plots/`: Diagnostic, parameter, and prediction plots

- `2_npe/`: Neural Posterior Estimation results (if enabled)

- `3_results/`: Final combined results

## Examples

``` r
if (FALSE) { # \dontrun{
# Set MOSAIC root (once per session or in ~/.Rprofile)
options(MOSAIC.root = "~/MOSAIC")

# Basic usage with defaults
result <- run_mosaic(
  iso_code = "ETH",
  dir_output = "~/results/ethiopia_2025"
)

# Multi-country calibration with custom settings
ctrl <- mosaic_run_defaults()
ctrl$parallel$n_cores <- 16
ctrl$calibration$target_r2 <- 0.95
ctrl$npe$enable <- TRUE

result <- run_mosaic(
  iso_code = c("MOZ", "MWI", "ZMB", "ZWE"),
  dir_output = "~/results/four_countries",
  n_iter = 5,
  n_sims = "auto",
  control = ctrl
)

# Fixed-size run for testing (CSV output for inspection)
ctrl <- mosaic_run_defaults()
ctrl$io <- mosaic_io_presets("debug")  # CSV format

result <- run_mosaic(
  iso_code = "ETH",
  dir_output = "~/results/test_run",
  n_iter = 1,
  n_sims = 1000,  # Exactly 1000 simulations
  control = ctrl
)

# Custom priors for sensitivity analysis
priors_tight <- get_location_priors("ETH")
priors_tight$tau_i$shape <- 20  # Tighter transmission rate prior

result <- run_mosaic(
  iso_code = "ETH",
  dir_output = "~/results/sensitivity_tight_tau",
  priors = priors_tight
)
} # }
```
