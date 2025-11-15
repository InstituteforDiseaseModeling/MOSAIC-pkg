# Install R and Python Dependencies for MOSAIC

Sets up a conda environment for MOSAIC using the `environment.yml` file
located in the package. The environment is stored in a fixed directory
("~/.virtualenvs/r-mosaic") and installs the LASER disease transmission
model simulation tool. This function also installs the R packages
`keras3` and `tensorflow`, and ensures that the Keras + TensorFlow
Python backend is available for use in R.

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
