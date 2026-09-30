# Install R and Python Dependencies for MOSAIC

Sets up a conda environment for MOSAIC using the `environment.yml` file
located in the package. The environment is stored in a fixed directory
("~/.virtualenvs/r-mosaic") and exists solely for the
environmental-suitability model: it installs the Keras + TensorFlow
Python backend into that environment. It installs the R packages
`reticulate` and `yaml` if they are missing, but it does **not** install
the R package `keras3` (a Suggests dependency), which
[`est_suitability()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_suitability.md)
also needs: install it separately with `install.packages("keras3")`.
Simulation and calibration are pure R and need none of it.

## Usage

``` r
install_dependencies(force = FALSE)
```

## Arguments

- force:

  Logical. If TRUE, deletes and recreates the conda environment from
  scratch. Default is FALSE.

## Value

No return value
