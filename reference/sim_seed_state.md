# Seed t=0 state from the validated parameters

Mirrors the Python components' `__init__` methods, each of which writes
its own compartment's `[0]` row from the matching `*_j_initial`
parameter.

## Usage

``` r
sim_seed_state(state, par)
```

## Arguments

- state:

  State environment from
  [`sim_alloc_state()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/sim_alloc_state.md).

- par:

  Parameters from
  [`sim_params()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/sim_params.md).

## Value

The state environment with row 1 seeded.
