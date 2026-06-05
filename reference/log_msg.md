# Simple Logging Function with Timestamps

A utility function for logging messages with ISO 8601 timestamps to both
the console and an optional log file. Used throughout
[`run_MOSAIC`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
and its helpers.

## Usage

``` r
log_msg(msg, ...)
```

## Arguments

- msg:

  Character string. The message to log. May include sprintf-style
  formatting placeholders (e.g., `"%d"`, `"%s"`, `"%.2f"`).

- ...:

  Additional arguments passed to `sprintf` for message formatting.

## Value

Invisible NULL. Function is called for its side effects.

## Details

Timestamps are emitted in ISO 8601 format (e.g.
`"2026-06-05T14:32:01-0700"`) so that a downstream consumer (a human
tail, a monitoring AI agent, a log shipper) can parse elapsed time
without ambiguity.

Lines are written to stdout and, if a log file path is known, also
appended to that file. The path is resolved in this order:

1.  `getOption("MOSAIC.log_file")` — preferred. Set this once at the
    start of a pipeline
    ([`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
    does this automatically) and every `log_msg` call — including those
    inside helper functions whose calling frame has no `dir_output`
    variable — will write to the same file.

2.  A `dir_output` binding in the calling frame (legacy fallback). The
    file path becomes `<dir_output>/run.log`. This preserves backward
    compatibility for callers that don't set the option.

If neither source resolves a path, lines go to stdout only.

## Examples

``` r
if (FALSE) { # \dontrun{
# Recommended: set the log file once at the start of a long-running
# pipeline, then every helper's log_msg() call lands in the same file.
options(MOSAIC.log_file = file.path(tempdir(), "run.log"))
on.exit(options(MOSAIC.log_file = NULL), add = TRUE)

log_msg("Starting analysis")
log_msg("Processing %d simulations with %d cores", 100, 8)
log_warn("Worker %d failed: %s", 17L, "OOM")
log_fatal("Aborting — convergence target unreachable")
} # }
```
