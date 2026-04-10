# Build Complete MOSAIC Control Structure

Creates a complete control structure for
[`run_mosaic()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md).
This is the primary interface for configuring MOSAIC execution settings,
consolidating calibration strategy, parameter sampling, parallelization,
and output options.

**Parameters are organized in workflow order:**

1.  `calibration`: How to run (simulations, iterations, batch sizes)

2.  `sampling`: What to sample (which parameters to vary)

3.  `likelihood`: How to score model fit (likelihood components and
    weights)

4.  `targets`: When to stop (ESS convergence thresholds)

5.  `fine_tuning`: Advanced calibration (adaptive batch sizing)

6.  `parallel`: Infrastructure (cores, cluster type)

7.  `io`: Output format (file format, compression)

8.  `paths`: File management (output directories, plots)

## Usage

``` r
mosaic_control_defaults(
  calibration = NULL,
  sampling = NULL,
  likelihood = NULL,
  targets = NULL,
  fine_tuning = NULL,
  predictions = NULL,
  weights = NULL,
  parallel = NULL,
  io = NULL,
  paths = NULL,
  logging = NULL
)
```

## Arguments

- calibration:

  List of calibration settings. Default is:

  - `n_simulations`: NULL for auto mode, or integer for fixed mode

  - `n_iterations`: Number of LASER iterations per simulation (default:
    3L)

  - `max_simulations_total`: Maximum total simulations across all phases
    (default: 100000L)

  - `batch_size_adaptive`: Simulations per batch in Phase 1 adaptive
    calibration (default: 500L)

  - `min_batches_adaptive`: Minimum Phase 1 batches before convergence
    check (default: 5L)

  - `max_batches_adaptive`: Maximum Phase 1 batches (default: 8L)

  - `max_batch_predictive`: Cap on single Phase 2 predictive batch
    (default: 10000L)

  - `max_batches_fine_tuning`: Maximum Phase 3 fine-tuning batches
    (default: 20L)

  - `target_r2_adaptive`: ESS regression R-squared target for Phase 1
    convergence (default: 0.90)

- sampling:

  List of parameter sampling flags (what to sample). Default is:

  - `sample_tau_i`: Sample transmission rate (default: TRUE)

  - `sample_mobility_gamma`: Sample mobility gamma (default: TRUE)

  - `sample_mobility_omega`: Sample mobility omega (default: TRUE)

  - `sample_mu_j`: Sample recovery rate (default: TRUE)

  - `sample_iota`: Sample importation rate (default: TRUE)

  - `sample_gamma_2`: Sample second dose efficacy (default: TRUE)

  - `sample_alpha_1`: Sample first dose efficacy (default: TRUE)

  - ... (see `mosaic_control_defaults()` for complete list of 38
    parameters)

- likelihood:

  List of likelihood calculation settings (how to score model fit).
  Default is:

  - `weight_cases`: Weight for cases vs deaths (default: 1.0)

  - `weight_deaths`: Weight for deaths vs cases (default: 1.0)

  - `weight_wis`: WIS regularizer weight (default: 0, try 0.10)

  - ... (see `mosaic_control_defaults()` for complete list)

- targets:

  List of convergence targets (when to stop). Default is:

  - `ESS_param`: Target ESS per parameter (default: 100)

  - `ESS_param_prop`: Proportion of parameters meeting ESS (default:
    0.95)

  - `ESS_best`: Target for both subset size and ESS within subset
    (default: 100).

  - `A_best`: Target agreement index (default: 0.70). Lower values allow
    top sims to dominate.

  - `CVw_best`: Target CV of weights (default: 1.0). Higher values
    permit sharper discrimination.

  - `percentile_min`: Minimum percentile for best subset search
    (default: 0.001)

  - `ESS_method`: ESS calculation method, "kish" or "perplexity"
    (default: "kish")

- fine_tuning:

  List of fine-tuning batch sizes (advanced calibration). Default is:

  - `batch_sizes`: Named list with massive, large, standard, precision,
    final

- predictions:

  List of prediction generation settings. Default is:

  - `ensemble_n_sims_per_param`: Stochastic runs per parameter set
    (default: 5L)

  The number of parameter sets in the ensemble is determined by the best
  subset (all sims with non-zero importance weights).

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
[`run_mosaic()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md).

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
    max_simulations_total = 50000,
    batch_size_adaptive = 1000
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

# Enable WIS regularizer and peak timing
ctrl <- mosaic_control_defaults(
  likelihood = list(
    weight_wis = 0.10,
    weight_peak_timing = 0.25
  )
)

# Use perplexity method for ESS calculations
ctrl <- mosaic_control_defaults(
  targets = list(ESS_method = "perplexity")
)

# Full workflow configuration (demonstrates logical order)
ctrl <- mosaic_control_defaults(
  calibration = list(n_simulations = NULL, n_iterations = 3),      # How to run
  sampling = list(sample_tau_i = TRUE, sample_mu_j = TRUE),        # What to sample
  likelihood = list(weight_wis = 0.10, weight_cases = 1.0),        # How to score
  targets = list(ESS_param = 100, ESS_param_prop = 0.95),          # When to stop
  fine_tuning = list(batch_sizes = list(final = 200)),             # Advanced calibration
  parallel = list(enable = TRUE, n_cores = 16),                    # Infrastructure
  io = mosaic_io_presets("default"),                               # Output format
  paths = list(clean_output = FALSE, plots = TRUE)                 # File management
)
```
