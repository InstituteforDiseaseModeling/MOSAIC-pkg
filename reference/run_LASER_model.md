# Run LASER Model Simulation

A wrapper function to execute a LASER model simulation via the Python
interface (reticulate). Automatically suppresses NumPy divide-by-zero
warnings that can occur during vaccination calculations when susceptible
and exposed compartments are both zero.

## Usage

``` r
run_LASER_model(
  paramfile,
  seed = 123L,
  visualize = FALSE,
  pdf = FALSE,
  outdir = tempdir(),
  py_module = NULL
)
```

## Arguments

- paramfile:

  Character. Path to the LASER parameter file (e.g., YAML or JSON) used
  to configure the simulation.

- seed:

  Integer. Random seed for reproducibility. Defaults to 123L.

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
  NULL, the module is imported via reticulate::import("laser").

## Value

A Python object (reticulate) representing the LASER model simulation
results.

## Examples

``` r
if (FALSE) { # \dontrun{
# Run with default settings:
result <- run_LASER_model(
  paramfile = "path/to/laser_params.yml",
  seed      = 20250418L,
  visualize = FALSE,
  pdf       = FALSE,
  outdir    = "./laser_output"
)

# Inspect results:
print(result)
} # }
```
