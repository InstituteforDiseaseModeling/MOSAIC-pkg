# Run MOSAIC Calibration Workflow (Advanced Interface)

**Advanced interface with full control over model specification.** For
most users,
[`run_mosaic_iso()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_mosaic_iso.md)
provides a simpler interface.

This function accepts pre-configured config and priors objects,
allowing:

- Completely custom configs (non-standard locations, custom data)

- Fine-grained control over all model parameters

- Testing with synthetic configurations

Executes the full MOSAIC calibration workflow:

1.  Adaptive calibration with R² convergence detection

2.  Single predictive batch (calculated from calibration phase)

3.  Adaptive fine-tuning with 5-tier batch sizing

4.  Post-hoc subset optimization for NPE priors

5.  Posterior quantile and distribution estimation

6.  Posterior predictive checks and uncertainty quantification

7.  Optional: Neural Posterior Estimation (NPE) stage

## Usage

``` r
run_MOSAIC(config, priors, dir_output, control = NULL, resume = FALSE)

run_mosaic(config, priors, dir_output, control = NULL, resume = FALSE)
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

- resume:

  Logical. If `TRUE`, continues from existing checkpoint. Default:
  FALSE.

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

- npe:

  enable, weight_strategy

- io:

  format, compression, compression_level

## Output Files

Results are organized in a structured directory tree:

- `0_setup/`: Configuration files (JSON format)

- `1_bfrs/outputs/`: Simulation results (Parquet format)

- `1_bfrs/diagnostics/`: ESS metrics, convergence results

- `1_bfrs/posterior/`: Posterior quantiles and distributions

- `1_bfrs/plots/`: Diagnostic, parameter, and prediction plots

- `2_npe/`: Neural Posterior Estimation results (if enabled)

- `3_results/`: Final combined results

## See also

[`run_mosaic_iso()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_mosaic_iso.md)
for simple interface with ISO codes

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
