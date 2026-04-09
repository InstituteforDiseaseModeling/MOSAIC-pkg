# Run LASER Model Simulation

A wrapper function to execute a LASER model simulation via the Python
interface (reticulate). Automatically suppresses NumPy divide-by-zero
warnings that can occur during vaccination calculations when susceptible
and exposed compartments are both zero.

## Usage

``` r
run_LASER(
  config,
  seed = NULL,
  quiet = FALSE,
  visualize = FALSE,
  pdf = FALSE,
  outdir = tempdir(),
  py_module = NULL
)

run_laser(
  config,
  seed = NULL,
  quiet = FALSE,
  visualize = FALSE,
  pdf = FALSE,
  outdir = tempdir(),
  py_module = NULL
)
```

## Arguments

- config:

  Character or list. Either a path to a LASER configuration file
  (YAML/JSON) or a configuration list object.

- seed:

  Integer or NULL. Random seed for reproducibility. If NULL (default),
  uses `config$seed` when present, otherwise defaults to 123L.

- quiet:

  Logical. If TRUE, suppress the progress bar during model execution.
  Defaults to FALSE.

- visualize:

  Logical. If TRUE, generate and display visualizations during the run.
  Defaults to FALSE.

- pdf:

  Logical. If TRUE, save visualizations as PDF files. Defaults to FALSE.

- outdir:

  Character. Directory where LASER outputs (e.g., logs, results) will be
  written. Defaults to a temporary directory.

- py_module:

  Python module. An optional pre-loaded reticulate LASER module. If
  NULL, the module is imported via
  reticulate::import("laser.cholera.metapop.model").

## Value

A Python object (reticulate) representing the LASER model simulation
results.

## Examples

``` r
if (FALSE) { # \dontrun{
# Run with config file path:
result <- run_LASER(
  config    = "path/to/laser_params.yml",
  seed      = 20250418L,
  quiet     = FALSE,
  visualize = FALSE,
  pdf       = FALSE,
  outdir    = "./laser_output"
)

# Run with config object (uses config$seed if present, else 123L):
result <- run_LASER(
  config = config_default,
  quiet  = TRUE
)

# Inspect results:
print(result)
} # }
```
