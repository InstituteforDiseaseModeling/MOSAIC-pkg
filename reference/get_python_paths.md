# Get Python Environment Paths

Returns the paths related to the desired MOSAIC Python environment

## Usage

``` r
get_python_paths()
```

## Value

A list with the following named elements:

- env:

  The full absolute path to the desired Python environment directory
  (the root folder of the virtual environment).

- exec:

  The expected path to the Python executable within that environment,
  constructed using OS-specific conventions.

- norm:

  The normalized (canonical) absolute path of the Python executable with
  symbolic links and/or relative components resolved.
