# Remove the MOSAIC Python Environment

This function checks whether the MOSAIC Python conda environment exists
at "~/.MOSAIC_conda_env". If it exists, it unlinks it from `reticulate`,
removes the directory, and resets the RETICULATE_PYTHON global variable.

## Usage

``` r
remove_python_env(force = FALSE)
```

## Arguments

- force:

  Logical. If TRUE, removes the environment without prompting for
  confirmation. Default is FALSE.

## Value

No return value.
