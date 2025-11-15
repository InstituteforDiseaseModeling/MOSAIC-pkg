# Detach MOSAIC Python Environment

Detaches the r-mosaic Python environment from the current R session.
This function provides guided instructions for properly detaching and
restarting R to use a different Python environment.

## Usage

``` r
detach_mosaic_env(restart = FALSE)
```

## Arguments

- restart:

  Logical. If TRUE and in RStudio, automatically restart the R session.
  Default: FALSE (just provide instructions).

## Value

Invisible NULL. If restart = TRUE and in RStudio, the session will
restart and this function will not return.

## Details

Due to reticulate's design, once Python is initialized in an R session,
it cannot be switched to a different environment without restarting R.

This function:

- Unsets the RETICULATE_PYTHON environment variable

- Provides instructions for restarting R

- Optionally triggers automatic restart (RStudio only)

After detaching:

- The RETICULATE_PYTHON variable is cleared

- The current Python session remains active (cannot be changed)

- When R restarts, MOSAIC will NOT auto-attach to r-mosaic

- You can set a different RETICULATE_PYTHON before loading MOSAIC

## See also

[`attach_mosaic_env`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/attach_mosaic_env.md)
for attaching the environment,
[`check_python_env`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/check_python_env.md)
for checking the current environment

## Examples

``` r
if (FALSE) { # \dontrun{
# Detach and get restart instructions
detach_mosaic_env()

# Detach and automatically restart (RStudio only)
detach_mosaic_env(restart = TRUE)

# To use a different Python after detaching:
# 1. detach_mosaic_env()
# 2. Restart R
# 3. Sys.setenv(RETICULATE_PYTHON = "/path/to/other/python")
# 4. library(MOSAIC)  # Will use your specified Python
} # }
```
