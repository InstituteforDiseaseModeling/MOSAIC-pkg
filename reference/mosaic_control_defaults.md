# Build Complete MOSAIC Control Structure

Creates a complete control structure for
[`run_mosaic()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_mosaic.md)
and
[`run_mosaic_iso()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_mosaic_iso.md).
This is the primary interface for configuring MOSAIC execution settings,
consolidating calibration strategy, parameter sampling, parallelization,
and output options.

**Parameters are organized in workflow order:**

1.  `calibration`: How to run (simulations, iterations, batch sizes)

2.  `sampling`: What to sample (which parameters to vary)

3.  `targets`: When to stop (ESS convergence thresholds)

4.  `fine_tuning`: Advanced calibration (adaptive batch sizing)

5.  `npe`: Post-calibration stage (neural posterior estimation)

6.  `parallel`: Infrastructure (cores, cluster type)

7.  `io`: Output format (file format, compression)

8.  `paths`: File management (output directories, plots)

## Usage

``` r
mosaic_control_defaults(
  calibration = NULL,
  sampling = NULL,
  targets = NULL,
  fine_tuning = NULL,
  npe = NULL,
  parallel = NULL,
  io = NULL,
  paths = NULL
)
```

## Arguments

- calibration:

  List of calibration settings. Default is:

  - `n_simulations`: NULL for auto mode, or integer for fixed mode

  - `n_iterations`: Number of LASER iterations per simulation (default:
    3L)

  - `max_simulations`: Maximum total simulations in auto mode (default:
    100000L)

  - `batch_size`: Simulations per batch in calibration phase (default:
    500L)

  - `min_batches`: Minimum calibration batches (default: 5L)

  - `max_batches`: Maximum calibration batches (default: 8L)

  - `target_r2`: R² target for calibration convergence (default: 0.90)

- sampling:

  List of parameter sampling flags (what to sample). Default is:

  - `sample_tau_i`: Sample transmission rate (default: TRUE)

  - `sample_mobility_gamma`: Sample mobility gamma (default: TRUE)

  - `sample_mobility_omega`: Sample mobility omega (default: TRUE)

  - `sample_mu_j`: Sample recovery rate (default: TRUE)

  - `sample_iota`: Sample importation rate (default: TRUE)

  - `sample_gamma_2`: Sample second dose efficacy (default: TRUE)

  - `sample_alpha_1`: Sample first dose efficacy (default: FALSE)

- targets:

  List of convergence targets (when to stop). Default is:

  - `ESS_param`: Target ESS per parameter (default: 500)

  - `ESS_param_prop`: Proportion of parameters meeting ESS (default:
    0.95)

  - `ESS_best`: Target ESS for best subset (default: 100)

  - `A_best`: Target agreement index (default: 0.95)

  - `CVw_best`: Target CV of weights (default: 0.5)

  - `B_min`: Minimum best subset size (default: 30)

  - `percentile_max`: Maximum percentile for best subset (default: 5.0)

- fine_tuning:

  List of fine-tuning batch sizes (advanced calibration). Default is:

  - `batch_sizes`: Named list with massive, large, standard, precision,
    final

- npe:

  List of NPE settings (post-calibration stage). Default is:

  - `enable`: Enable NPE training (default: FALSE)

  - `weight_strategy`: NPE weight strategy (default: "continuous_all")

- parallel:

  List of parallelization settings (infrastructure). Default is:

  - `enable`: Enable parallel execution (default: FALSE)

  - `n_cores`: Number of cores to use (default: 1L)

  - `type`: Cluster type, "PSOCK" or "FORK" (default: "PSOCK")

  - `progress`: Show progress bar (default: TRUE)

- io:

  List of I/O settings (output format). Default is:

  - `format`: Output format, "parquet" or "csv" (default: "parquet")

  - `compression`: Compression algorithm (default: "zstd")

  - `compression_level`: Compression level (default: 3L)

- paths:

  List of path and output settings (file management). Default is:

  - `clean_output`: Remove output directory if exists (default: FALSE)

  - `plots`: Generate diagnostic plots (default: TRUE)

## Value

A complete control list suitable for passing to
[`run_mosaic()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_mosaic.md)
or
[`run_mosaic_iso()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_mosaic_iso.md).

## Examples

``` r
# Default control settings
ctrl <- mosaic_control_defaults()

# Quick parallel run with 8 cores
ctrl <- mosaic_control_defaults(
  parallel = list(enable = TRUE, n_cores = 8)
)

# Fixed mode with 5000 simulations, 5 iterations each
ctrl <- mosaic_control_defaults(
  calibration = list(
    n_simulations = 5000,
    n_iterations = 5
  )
)

# Auto mode with custom settings
ctrl <- mosaic_control_defaults(
  calibration = list(
    n_simulations = NULL,  # NULL = auto mode
    n_iterations = 3,
    max_simulations = 50000,
    batch_size = 1000
  ),
  parallel = list(enable = TRUE, n_cores = 16)
)

# Only sample specific parameters
ctrl <- mosaic_control_defaults(
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

# Full workflow configuration (demonstrates logical order)
ctrl <- mosaic_control_defaults(
  calibration = list(n_simulations = NULL, n_iterations = 3),  # How to run
  sampling = list(sample_tau_i = TRUE, sample_mu_j = TRUE),    # What to sample
  targets = list(ESS_param = 500, ESS_param_prop = 0.95),      # When to stop
  fine_tuning = list(batch_sizes = list(final = 200)),         # Advanced calibration
  npe = list(enable = TRUE, weight_strategy = "best_subset"),  # Post-calibration
  parallel = list(enable = TRUE, n_cores = 16),                # Infrastructure
  io = mosaic_io_presets("default"),                           # Output format
  paths = list(clean_output = FALSE, plots = TRUE)             # File management
)
```
