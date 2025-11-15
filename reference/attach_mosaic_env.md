# Attach MOSAIC Python Environment

Explicitly attaches the r-mosaic Python environment to the current R
session. This function initializes Python with the r-mosaic environment,
making all MOSAIC Python dependencies (laser_cholera, torch, zuko, etc.)
available.

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

This function is called automatically when MOSAIC is loaded (via
.onAttach). You typically don't need to call it manually unless:

- You want to verify the Python environment is working

- You detached and want to re-attach without restarting R

- You're writing a script that needs explicit environment control

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
