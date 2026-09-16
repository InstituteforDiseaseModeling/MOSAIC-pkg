# Assemble engine results in the downstream contract

Equivalent of the Python engine's `RInterface`. State is held time-major
(one environment per tick, each holding every channel); results are
patch-major (`[npatches, nticks]`). The three trimming rules are not
interchangeable and are reproduced exactly from `model.py:82-165`:

## Usage

``` r
sim_results(state, par)

SIM_CHANNELS_TRIM_FIRST

SIM_CHANNELS_TRIM_LAST

SIM_CHANNELS_TRANSPOSE_ONLY

SIM_CHANNELS_PRECOMPUTED

SIM_CHANNELS_PASSTHROUGH

SIM_CHANNELS

SIM_CHANNELS_INTEGER

SIM_CHANNELS_DOUBLE
```

## Arguments

- state:

  State environment from the run loop.

- par:

  Parameters from
  [`sim_params()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/sim_params.md).

## Value

Named list of `[npatches, nticks]` matrices.

## Details

- compartments, incidence, hazards, `N`, `W` drop the `t = 0` seed row
  (`[1:, :].T`);

- event counts (births, deaths, reported) drop the *last* row
  (`[:-1, :].T`), because they are written at `tick` rather than
  `tick + 1`;

- doses and the precomputed beta/delta matrices are already
  `nticks`-shaped and are only transposed.

Storage mode is per field: counts are `integer`, rates and hazards are
`double`. No dimnames – the Python return has none and no consumer reads
any.
