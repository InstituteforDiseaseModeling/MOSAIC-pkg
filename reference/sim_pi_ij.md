# Row-stochastic gravity connectivity matrix

`x_ij = N_j^omega * d_ij^(-gamma)` for `i != j`, row-normalised. The
diagonal is zero: there is no self-mobility. The migrating fraction
`tau_i` is deliberately *not* applied here — the Python engine factors
it in at runtime inside `HumanToHuman.__call__` so it can vary per patch
without rebuilding `pi_ij`, and that split is preserved.

## Usage

``` r
sim_pi_ij(N, d, omega, gamma)
```

## Arguments

- N:

  Numeric vector of initial patch populations, length `npatches`.

- d:

  Distance matrix from
  [`sim_distance_matrix()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/sim_distance_matrix.md).

- omega, gamma:

  Gravity-model exponents (`mobility_omega`, `mobility_gamma`).

## Value

An `[npatches, npatches]` row-stochastic matrix.
