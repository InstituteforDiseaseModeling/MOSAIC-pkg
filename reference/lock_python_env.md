# Lock Python Environment to r-mosaic

Ensures the r-mosaic Python environment is active and prevents
switching. This function should be called at the start of parallel
workflows or any script that requires stable Python environment access.

The function:

- Verifies the r-mosaic environment exists

- Sets and locks the RETICULATE_PYTHON environment variable

- Initializes Python with the r-mosaic environment

- Imports laser_cholera to ensure it's available

- Fails loudly if any step fails

## Usage

``` r
lock_python_env()
```

## Value

Invisible TRUE if successful, stops with error otherwise

## Details

Once Python is initialized with a specific environment in an R session,
reticulate prevents switching to a different environment. This function
leverages that behavior by initializing with r-mosaic first, effectively
locking the session to that environment.

Best practice: Call this function immediately after loading the MOSAIC
package and before any other operations that might initialize Python.

## See also

[`check_python_env`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/check_python_env.md)
for checking the current environment,
[`use_mosaic_env`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/use_mosaic_env.md)
for switching to MOSAIC environment,
[`check_dependencies`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/check_dependencies.md)
for verifying the setup,
[`install_dependencies`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/install_dependencies.md)
for installing the environment

## Examples

``` r
if (FALSE) { # \dontrun{
# At the start of a calibration script
library(MOSAIC)
set_root_directory("/path/to/MOSAIC")

# Lock Python environment before creating cluster
lock_python_env()

# Now safe to create cluster and run simulations
cl <- makeCluster(9)
# ... rest of script
} # }
```
