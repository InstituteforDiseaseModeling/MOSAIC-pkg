# Check Python Environment for MOSAIC

Displays information about the Python environment currently attached to
the R session and checks whether it matches the MOSAIC Python
environment at ~/.virtualenvs/r-mosaic. Provides clickable links to
relevant functions based on the environment status.

The Python environment is OPTIONAL. Since v0.68.0 the transmission
engine is pure R, so simulation and calibration never initialise Python;
the environment exists only for
[`est_suitability`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_suitability.md).
A FALSE return is therefore not an error unless you are re-fitting the
suitability model.

## Usage

``` r
check_python_env()
```

## Value

Logical. TRUE if the r-mosaic Python environment is linked to the
current session, FALSE otherwise.
