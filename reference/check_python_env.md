# Check Python Environment for MOSAIC

Displays information about the Python environment currently attached to
the R session and checks whether it matches the MOSAIC Python
environment at ~/.virtualenvs/r-mosaic. Provides clickable links to
relevant functions based on the environment status.

## Usage

``` r
check_python_env()
```

## Value

Logical. TRUE if the r-mosaic Python environment is linked to the
current session, FALSE otherwise.
