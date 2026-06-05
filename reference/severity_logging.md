# Severity-tagged logging helpers

Thin wrappers around
[`log_msg`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/log_msg.md)
that prepend a severity tag (`[INFO]`, `[WARN]`, `[ERROR]`, `[FATAL]`)
after the timestamp. Use these in place of bare prefixes (e.g.
`"WARNING:"`, `"ERROR:"`) so that a downstream consumer can filter by
severity with a single regex.

## Usage

``` r
log_info(msg, ...)

log_warn(msg, ...)

log_error(msg, ...)

log_fatal(msg, ...)
```

## Arguments

- msg:

  Character string with optional sprintf-style placeholders.

- ...:

  Additional arguments passed to sprintf.

## Value

Invisible NULL.

## Details

`log_fatal` is intended to be called immediately before a
[`stop()`](https://rdrr.io/r/base/stop.html) or an early-return failure
exit — it records the terminal status in the log file so that a tailing
process can grep one canonical line instead of guessing whether the
pipeline aborted.

## Examples

``` r
if (FALSE) { # \dontrun{
log_info("Calibration converged after %d batches", 12L)
log_warn("Worker %d returned non-finite likelihood; dropping", 33L)
log_error("client$gather() failed: %s", "ConnectionError")
log_fatal("Best-seed sample_parameters failed; cannot continue")
} # }
```
