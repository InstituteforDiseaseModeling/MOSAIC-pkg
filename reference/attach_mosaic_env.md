# Attach MOSAIC Python Environment

Explicitly attaches the r-mosaic Python environment to the current R
session. This function initializes Python with the r-mosaic environment,
making the MOSAIC Python dependencies (tensorflow/keras, numpy)
available. Those serve the environmental-suitability model only –
simulation and calibration are pure R since v0.68.0 and need no Python
at all.

The function:

- Sets the RETICULATE_PYTHON environment variable

- Initializes Python with r-mosaic

- Verifies the environment is working

- Provides clear error messages if attachment fails

## Usage

``` r
attach_mosaic_env(silent = FALSE)
```

## Arguments

- silent:

  Logical. If TRUE, suppresses informational messages. Default: FALSE.

## Value

Invisible TRUE if successful, stops with error otherwise

## Details

Loading MOSAIC does *not* attach Python (since v0.78.0). `.onLoad` only
sets `RETICULATE_PYTHON` to the r-mosaic interpreter when that variable
is unset and the interpreter exists, so reticulate binds r-mosaic lazily
on the first Python call (e.g. the keras3 calls in
[`est_suitability()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_suitability.md)).
To use a different interpreter, set `RETICULATE_PYTHON` before
[`library(MOSAIC)`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/).
Call this function explicitly when:

- You want Python initialised up front, before
  [`est_suitability()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_suitability.md)

- You want to verify the Python environment is working

- You detached and want to re-attach without restarting R

Once Python is initialized, reticulate prevents switching to a different
environment without restarting R. If Python is already initialized with
a different environment, this function will fail with an error message
instructing you to restart R.

## See also

[`detach_mosaic_env`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/detach_mosaic_env.md)
for detaching the environment,
[`check_python_env`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/check_python_env.md)
for checking the current environment,
[`check_dependencies`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/check_dependencies.md)
for verifying the setup,
[`install_dependencies`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/install_dependencies.md)
for installing the environment

## Examples

``` r
if (FALSE) { # \dontrun{
# Manually attach MOSAIC environment
attach_mosaic_env()

# Attach silently (no messages)
attach_mosaic_env(silent = TRUE)

# Verify after attaching
check_python_env()
} # }
```
