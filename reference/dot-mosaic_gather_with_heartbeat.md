# Gather Dask futures while emitting periodic progress lines

Wraps `client$gather(futures)` with a polling loop that uses
`dask.distributed.wait(futures, timeout=interval_sec)` to wake up every
`interval_sec` seconds, count completed vs pending futures, and emit a
single structured `[PROGRESS]` log line per heartbeat. Eliminates the
silent-during-gather window that otherwise leads a tailing operator
(human or AI) to wrongly conclude the pipeline has hung.

## Usage

``` r
.mosaic_gather_with_heartbeat(
  client,
  futures,
  log_fn = log_msg,
  phase = "gather",
  interval_sec = 30L
)
```

## Arguments

- client:

  A reticulated `dask.distributed.Client` object.

- futures:

  List of Dask future objects. Empty list returns empty list.

- log_fn:

  Logging function compatible with `log_msg(msg, ...)`.

- phase:

  Short slug for the `phase=` field of the progress line (e.g.
  `"calibration_batch"`, `"postca_ensemble"`).

- interval_sec:

  Heartbeat interval. Defaults to 30 s — long enough that the log isn't
  spammed during fast gathers, short enough that a hung gather is
  detected within a minute.

## Value

The list returned by `client$gather(futures)`.

## Details

On wait/gather error, the helper does NOT swallow — it lets the caller's
tryCatch handle diagnostics (e.g. first-future status inspection).
