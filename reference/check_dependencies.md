# Check Installed R and Python Dependencies for MOSAIC

This function checks the MOSAIC Python conda environment, verifies that
the expected Python packages are installed, and confirms that the R
packages `keras3` and `tensorflow` are present and configured correctly.
It prints the currently active Python configuration and confirms whether
the backend is working.

## Usage

``` r
check_dependencies()
```

## Value

No return value. Prints diagnostic messages about environment status and
package versions.
