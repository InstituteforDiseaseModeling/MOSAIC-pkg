# End-of-run derived diagnostics: spatial hazard and coupling

The tenth and last component of the pipeline (`derivedvalues.py`).
Unlike the other nine it computes nothing per tick: it is a whole-run
summary that fires once, on the final tick, and its two outputs
(`spatial_hazard`, `coupling`) are read by
[`calc_model_ensemble()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_ensemble.md)
and the spatial plots but by no other part of the simulation. Nothing
downstream of it is stochastic, and it consumes no randomness, so it
adds no draw sites.

## Usage

``` r
sim_phase_derived_values(state, par, ctl, tick)
```

## Which rows each quantity uses

The two are sliced differently, and the difference is load-bearing:
`spatial_hazard` is computed from the oracle's `[1:, :]` slices – the
seed row dropped, so column `t` is the state at the *end* of tick `t` –
while `coupling` is handed the untrimmed arrays and so correlates
`nticks + 1` observations, seed row included.

## Not the same as the exported analysis helpers

The package also exports
[`calc_spatial_hazard()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_spatial_hazard.md)
and
[`calc_spatial_correlation_matrix()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_spatial_correlation_matrix.md),
which compute quantities of the same names. They are **not**
interchangeable with these and must not be substituted for them:
[`calc_spatial_hazard()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_spatial_hazard.md)
counts waned vaccinees (`V1_sus`, `V2_sus`) in the susceptible pool and
zeroes the mobility diagonal itself, and
[`calc_spatial_correlation_matrix()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_spatial_correlation_matrix.md)
fills the diagonal with 1 unconditionally where the engine leaves a
constant patch undefined. Either substitution would change the engine's
output and break parity with the oracle. Neither helper has a production
caller today; they are analysis-side functions with their own
definitions.
