# Run MOSAIC Calibration by ISO Code (Simple Interface)

**This is the recommended interface for most users.** Simplified wrapper
that loads default config and priors for specified ISO codes and runs
the full MOSAIC calibration.

For advanced customization (custom configs, non-standard locations), use
[`run_mosaic()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_mosaic.md)
directly.

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
run_mosaic_iso(iso_code, dir_output, control = NULL, resume = FALSE)

run_MOSAIC_iso(iso_code, dir_output, control = NULL, resume = FALSE)
```

## Arguments

- iso_code:

  Character vector of ISO3 codes (e.g., `"ETH"` or
  `c("ETH", "KEN", "TZA")`). Determines which locations to model. Config
  and priors are automatically loaded for these locations.

- dir_output:

  Character. Output directory for this calibration run (REQUIRED). All
  results will be saved here. Must be unique per run.

- control:

  Control list created with
  [`mosaic_control_defaults()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/mosaic_control_defaults.md).
  If `NULL`, uses defaults. Common settings:

  - `calibration$n_simulations`: NULL for auto mode, integer for fixed

  - `calibration$n_iterations`: Iterations per simulation (default: 3)

  - `parallel$enable`: Enable parallel execution (default: FALSE)

  - `parallel$n_cores`: Number of cores (default: 1)

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

  Named list with run statistics

## See also

[`run_mosaic()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_mosaic.md)
for advanced usage with custom configs

[`mosaic_control_defaults()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/mosaic_control_defaults.md)
for building control structures

## Examples

``` r
if (FALSE) { # \dontrun{
# === SIMPLE USAGE ===

# Single location with defaults
run_mosaic_iso("ETH", "./output")

# Multiple locations
run_mosaic_iso(c("ETH", "KEN", "TZA"), "./output")

# === WITH CONTROL SETTINGS ===

# Parallel execution with 8 cores
run_mosaic_iso(
  iso_code = "ETH",
  dir_output = "./output",
  control = mosaic_control_defaults(
    parallel = list(enable = TRUE, n_cores = 8)
  )
)

# Fixed mode: exactly 5000 simulations, 5 iterations each
run_mosaic_iso(
  iso_code = "ETH",
  dir_output = "./output",
  control = mosaic_control_defaults(
    calibration = list(
      n_simulations = 5000,
      n_iterations = 5
    )
  )
)

# Auto mode with custom maximum
run_mosaic_iso(
  iso_code = "ETH",
  dir_output = "./output",
  control = mosaic_control_defaults(
    calibration = list(
      n_simulations = NULL,  # NULL = auto mode
      max_simulations = 50000,
      batch_size = 1000
    ),
    parallel = list(enable = TRUE, n_cores = 16)
  )
)

# === SAMPLE SPECIFIC PARAMETERS ===

# Only sample transmission and recovery, hold others fixed
run_mosaic_iso(
  iso_code = "ETH",
  dir_output = "./output",
  control = mosaic_control_defaults(
    sampling = list(
      sample_tau_i = TRUE,
      sample_mobility_gamma = FALSE,
      sample_mobility_omega = FALSE,
      sample_mu_j = TRUE,
      sample_iota = FALSE,
      sample_gamma_2 = FALSE,
      sample_alpha_1 = FALSE
    )
  )
)
} # }
```
