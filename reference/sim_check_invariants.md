# Assert per-tick invariants on engine state

These are checked independently of the Python oracle, which is the
point: they catch the class of bug where R and Python agree because both
are wrong, and they are the only correctness checks that survive once
the oracle is gone.

## Usage

``` r
sim_check_invariants(state, tick, compartments)
```

## Arguments

- state:

  State environment from
  [`sim_alloc_state()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/sim_alloc_state.md).

- tick:

  Tick index just written (1-based row into the state series).

- compartments:

  Character vector of compartments in play; `N` is checked against the
  sum of exactly these.

## Value

Invisibly `TRUE`; errors on violation.
