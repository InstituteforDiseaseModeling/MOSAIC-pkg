# Count active Dask workers via Python (snapshot-staleness-safe)

`client$scheduler_info()$workers` cannot be relied on to count Dask
workers because it is a STALE, lagging client-side snapshot of the
workers dict: when workers join over time (the Coiled spin-up case –
request 800, scale up gradually) the `workers` dict does not reflect the
live count and reports a too-low number (e.g. 5 while hundreds are
actually running). This was verified directly against a local
`LocalCluster`: scaling from 1 to 6 workers left
`len(scheduler_info()['workers'])` reporting 5 while every fresh source
(`client.nthreads()`, `scheduler_info()['n_workers']`, the scheduler's
own `len(s.workers)`) correctly reported 6. (An earlier comment here
attributed the bug to a reticulate-conversion "per-worker field count of
5"; that explanation was factually wrong – on a STATIC cluster
`len(scheduler_info()$workers)` returns the correct count. The real
cause is snapshot staleness of the `workers` dict as workers join.)

## Usage

``` r
.mosaic_count_dask_workers(client)
```

## Arguments

- client:

  A reticulated `dask.distributed.Client` object.

## Value

Integer worker count, or `NA_integer_` on error.

## Details

This helper reads a FRESH source server-side. PRIMARY:
`len(client.nthreads())` – a live scheduler RPC over a stable public API
that returns one entry per worker (chosen over
`client.run_on_scheduler(lambda s: len(s.workers))` because it needs no
lambda serialization, which was observed to fail in some environments,
and is a lighter call). FALLBACK 1: `scheduler_info()['n_workers']` when
present (also fresh, but counts threads when threads-per-worker \> 1).
FALLBACK 2: `len(scheduler_info().get('workers', {}))` (the stale
last-resort original behavior). The tiny Python helper returns a plain
`int`, which `py_run_string(..., convert = TRUE)` reliably converts to
an R integer; `-1` signals a Python-side failure that the R `tryCatch`
maps to `NA_integer_`.
