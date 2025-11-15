# Extract NPE-formatted simulated data from LASER model output

Converts LASER model output to the data frame format expected by NPE
posterior estimation. Handles both R list structure and Python object
structure from laser_cholera.

## Usage

``` r
get_npe_simulated_data(laser_result, verbose = FALSE)
```

## Arguments

- laser_result:

  Output from laser_cholera run_model

- verbose:

  Logical whether to print messages

## Value

Data frame with columns j (location), t (time), cases
