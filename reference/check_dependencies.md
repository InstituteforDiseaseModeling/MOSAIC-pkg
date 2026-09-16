# Check Installed R and Python Dependencies for MOSAIC

This function checks the MOSAIC Python conda environment, verifies that
the expected Python packages are installed, and confirms that the R
packages `keras3` and `tensorflow` are present and configured correctly.
It prints the currently active Python configuration and confirms whether
the backend is working.

Since v0.68.0 the transmission engine is pure R, so the Python
environment exists only for the environmental-suitability (psi) model.
What this function validates is therefore a TensorFlow environment, not
a simulation one: a broken Python env costs you
[`est_suitability()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_suitability.md),
not
[`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md).

## Usage

``` r
check_dependencies()
```

## Value

No return value. Prints diagnostic messages about environment status and
package versions.
