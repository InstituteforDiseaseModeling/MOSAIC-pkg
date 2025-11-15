# Prepare NPE Training Data from BFRS Results

Loads simulation outputs and parameters from BFRS results directory and
prepares them for Neural Posterior Estimation (NPE) training. Optimized
to load only individual output files for weighted simulations, avoiding
the need to read and filter a large combined outputs file.

## Usage

``` r
prepare_npe_data(
  bfrs_dir,
  results = NULL,
  param_names = NULL,
  verbose = TRUE,
  chunk_size = NULL
)
```

## Arguments

- bfrs_dir:

  Directory containing BFRS results with:

  - simulations.parquet: Parameter values and weights

  - outputs/timeseries/timeseries_NNNNNNN.parquet: Individual timeseries
    files

  - priors.json: Parameter prior distributions

- results:

  Optional data frame of BFRS results already loaded in memory (NULL =
  load from file). Preferred when results are already available to avoid
  redundant file I/O. Must contain columns: sim, likelihood, and
  parameter columns.

- param_names:

  Character vector of parameter names to extract (NULL = auto-detect)

- verbose:

  Logical, print progress messages (default: TRUE)

- chunk_size:

  Integer, chunk size for processing large datasets (NULL = auto)

## Value

List containing:

- parameters:

  Matrix of parameter values (n_sims × n_params)

- observations:

  Matrix of flattened time series (n_sims × n_features)

- weights:

  Vector of simulation weights

- bounds:

  Matrix of parameter bounds (n_params × 2)

- param_names:

  Character vector of parameter names

- n_samples, n_params, n_timesteps, n_locations:

  Integer dimensions

- observed_data:

  Optional data frame of observed data (if available)

## Details

This function implements a high-performance loading strategy:

1.  Loads simulations.parquet to identify weighted simulations

2.  Loads only the individual timeseries files for weighted simulations

3.  Combines loaded files using data.table::rbindlist() for efficiency

4.  Reshapes data from long to wide format for NPE training

Performance: For typical BFRS results with 1\\

- Old approach (combined file): 30-120 seconds to load and filter 45M
  rows

- New approach (individual files): 0.1-0.5 seconds to load 450K rows
  directly

- Speedup: 100-1200× faster

The function expects individual output files in the outputs/timeseries/
subdirectory. These files are created during simulation. The entire
outputs/ directory can be removed after NPE training completes to free
disk space.

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic usage (loads from file)
npe_data <- prepare_npe_data(
    bfrs_dir = "path/to/1_bfrs",
    param_names = c("beta_env", "beta_hum", "tau_i")
)

# Optimized usage (pass pre-loaded results)
results <- arrow::read_parquet("path/to/1_bfrs/outputs/simulations.parquet")
results$weight_npe <- get_npe_weights(results, strategy = "continuous_best")
npe_data <- prepare_npe_data(
    bfrs_dir = "path/to/1_bfrs",
    results = results,
    param_names = c("beta_env", "beta_hum", "tau_i")
)
} # }
```
