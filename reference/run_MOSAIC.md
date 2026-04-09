# Run MOSAIC Calibration Workflow

**Complete Bayesian calibration workflow with full control over model
specification.**

This function accepts pre-configured config and priors objects,
allowing:

- Completely custom configs (non-standard locations, custom data)

- Fine-grained control over all model parameters

- Testing with synthetic configurations

Executes the full MOSAIC calibration workflow:

1.  Adaptive calibration with R² convergence detection

2.  Single predictive batch (calculated from calibration phase)

3.  Adaptive fine-tuning with 5-tier batch sizing

4.  Post-hoc subset optimization

5.  Posterior quantile and distribution estimation

6.  Posterior predictive checks and uncertainty quantification

## Usage

``` r
run_MOSAIC(
  config,
  priors,
  dir_output,
  control = NULL,
  cluster = NULL,
  dask_spec = NULL
)

run_mosaic(
  config,
  priors,
  dir_output,
  control = NULL,
  cluster = NULL,
  dask_spec = NULL
)
```

## Arguments

- config:

  Named list of LASER model configuration (REQUIRED). Contains
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

  - `calibration$n_iterations`: LASER iterations per simulation
    (default: 3)

  - `calibration$max_simulations`: Maximum total simulations (default:
    100000)

  - `sampling`: Which parameters to sample vs hold fixed

  - `parallel`: Cluster settings for parallel execution

- cluster:

  Optional pre-built R parallel cluster from
  [`make_mosaic_cluster`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/make_mosaic_cluster.md).
  Only used when `dask_spec = NULL`. When provided, skips cluster
  creation and teardown, reusing existing workers. Useful for staged
  estimation where multiple `run_MOSAIC` calls share a cluster. When
  `dask_spec` is provided this argument is not used; the caller retains
  ownership of any cluster passed and is responsible for stopping it
  after `run_MOSAIC` returns.

- dask_spec:

  Optional named list specifying a Dask/Coiled cluster. When provided,
  simulations are dispatched to Dask workers instead of a local R
  parallel cluster. Required fields: `type` ("coiled" or "scheduler").
  Coiled fields: `n_workers`, `software`, `workspace` (required for
  multi-workspace Coiled accounts), `vm_types`, `scheduler_vm_types`,
  `region`, `idle_timeout`. Scheduler fields: `address`. Optional Coiled
  pass-through fields: `timeout`, `environ`, `scheduler_disk_size`,
  `worker_disk_size`, `scheduler_options`, `worker_options`,
  `spot_policy`, `host_setup_script`. When `NULL` (default) the local R
  parallel backend is used.

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

  n_simulations, n_iterations, max_simulations, batch_size, etc.

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
  # ... all other LASER parameters
)

custom_priors <- list(
  # ... custom prior specifications
)

run_MOSAIC(custom_config, custom_priors, "./output")
} # }
```
