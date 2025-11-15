# Calculate NPE Architecture Specification

Automatically determines optimal neural network architecture based on
problem dimensions, with v5.2 features including guards and ramping.

## Usage

``` r
calc_npe_architecture(
  n_sims,
  n_params,
  n_timesteps,
  n_locations,
  tier = "auto",
  verbose = FALSE
)
```

## Arguments

- n_sims:

  Number of simulations available

- n_params:

  Number of parameters to estimate

- n_timesteps:

  Number of time points in observations

- n_locations:

  Number of spatial locations

- tier:

  Architecture tier: "auto", "minimal", "small", "medium", "large",
  "xlarge"

- verbose:

  Print architecture details

## Value

List with architecture specification
