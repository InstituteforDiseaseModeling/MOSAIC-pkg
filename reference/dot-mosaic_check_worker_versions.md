# Compare orchestrator vs. Coiled-worker laser-cholera versions

Pure detection helper for the Dask/Coiled parity guard in
[`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md).
The remote worker path is pure Python and runs the laser-cholera engine
directly, so a version drift between the orchestrator's reticulate env
and the Coiled worker image silently changes simulation results. This
function detects such drift; the caller decides whether to abort or warn
(so it can be unit tested without a live cluster and never throws).

## Usage

``` r
.mosaic_check_worker_versions(local_version, worker_versions)
```

## Arguments

- local_version:

  Character scalar: the orchestrator's laser.cholera `__version__` (or
  NULL if it could not be determined).

- worker_versions:

  Named list keyed by worker address, each element a list with a
  `laser_cholera` entry (as returned by the worker
  `get_engine_versions()` via `client$run()`). May be empty.

## Value

A list with:

- ok:

  TRUE if no mismatch was proven (all workers match, or the check could
  not run).

- n_workers:

  Number of worker responses inspected.

- mismatches:

  Character vector describing each offending worker (empty when `ok` and
  no mismatches).

- note:

  Optional message when the check could not be performed (NULL local
  version or zero worker responses); caller should warn, not abort, in
  this case.
