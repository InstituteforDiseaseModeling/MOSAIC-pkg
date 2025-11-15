# Use MOSAIC Python Environment

**Note**: This function is maintained for backward compatibility. For
new code, use
[`attach_mosaic_env`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/attach_mosaic_env.md)
instead.

Switches the current R session to use the MOSAIC Python environment at
~/.virtualenvs/r-mosaic. This function attempts to switch from other
Python environments (like r-keras) to the MOSAIC environment.

## Usage

``` r
use_mosaic_env()
```

## Value

Invisible TRUE if successful, FALSE otherwise.

## Details

This function attempts to switch the Python environment mid-session. Due
to reticulate limitations, if Python has already been initialized with a
different environment, a complete switch may require restarting R. The
function will provide appropriate guidance based on the current state.

**Recommended**: Use
[`attach_mosaic_env`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/attach_mosaic_env.md)
for clearer semantics and better error handling. The Python environment
is now automatically attached when you load MOSAIC via
[`library(MOSAIC)`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/).

## See also

[`attach_mosaic_env`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/attach_mosaic_env.md)
for the recommended function,
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
# Old approach (still works)
use_mosaic_env()

# New approach (recommended)
attach_mosaic_env()

# Auto-attach on package load (no action needed)
library(MOSAIC)  # Automatically attaches r-mosaic
} # }
```
