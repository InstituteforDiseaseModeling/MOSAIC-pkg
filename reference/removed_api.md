# Removed MOSAIC functions and arguments

Names that MOSAIC used to export and no longer does. Each raises an
error saying what it was and what to call instead.

## Usage

``` r
lock_python_env(...)

run_LASER(...)

run_laser(...)

make_LASER_config(...)

get_default_LASER_config(...)
```

## Arguments

- ...:

  Ignored; present only so the stub accepts any old call shape.

## Details

Nothing is silently absorbed. The alternative – accepting an argument or
a name and quietly doing something else – is precisely the failure mode
of CLAUDE.md lesson \#13, where a back-compat shim silently reverted
user settings to defaults for fifteen versions with no warning. A loud
error is cheaper than a deprecation cycle and far safer than silence.

**v0.70.0 – the LASER naming.** `LASER` named the Python `laser-cholera`
package that MOSAIC used to shell out to. The engine has been pure R
since v0.68.0 and the dependency went in v0.69.0, so the name pointed at
something that no longer exists. `run_LASER()` is now
[`run_simulation`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_simulation.md),
`make_LASER_config()` is
[`make_simulation_config`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/make_simulation_config.md),
and `get_default_LASER_config()` – a byte-for-byte duplicate of
[`get_default_config`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_default_config.md)
with zero callers – is gone in favour of the latter.

**v0.67.0 – the Dask/Coiled backend.** Removed with the distributed
backend: the `dask_spec` argument to
[`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
/
[`run_rolling_cv()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_rolling_cv.md),
plus `check_coiled_workspace()` and `mosaic_dask_presets()` (both
deleted outright in v0.70.0, one minor version after the engine cutover,
as scheduled).
