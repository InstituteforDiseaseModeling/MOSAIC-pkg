# Create a draw controller for one simulation

Create a draw controller for one simulation

## Usage

``` r
sim_draws(
  mode = c("rng", "replay"),
  seed = NULL,
  record = NULL,
  tol_rel = 1e-06,
  tol_abs = 1e-09
)
```

## Arguments

- mode:

  Either `"rng"` (draw from R's generator) or `"replay"` (pop recorded
  draws from `record` and assert they match).

- seed:

  Integer seed. Used in `"rng"` mode.

- record:

  Replay record from
  [`sim_read_fixture()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/sim_fixture.md).
  Required when `mode = "replay"`.

- tol_rel, tol_abs:

  Combined tolerance for comparing draw parameters against the record:
  `abs(a - b) <= tol_abs + tol_rel * abs(b)`. A purely relative
  tolerance is wrong here because rates legitimately reach exactly zero
  in low-transmission patches.

## Value

A draw controller (an environment).
