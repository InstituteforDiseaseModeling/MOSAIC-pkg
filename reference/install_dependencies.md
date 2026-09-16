# Install R and Python Dependencies for MOSAIC

Sets up a conda environment for MOSAIC using the `environment.yml` file
located in the package. The environment is stored in a fixed directory
("~/.virtualenvs/r-mosaic") and exists solely for the
environmental-suitability model: it installs the R packages `keras3` and
`tensorflow` and ensures the Keras + TensorFlow Python backend is
available from R. Simulation and calibration are pure R and need none of
it.

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
