# Resolve the per-location NB dispersion for a calibration run

Called once by
[`run_MOSAIC`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
before the simulation loop. Estimates the dispersion for both channels
from the configured observations, or honours an explicit user override.

## Usage

``` r
.mosaic_resolve_nb_dispersion(config, control, score_window = NULL)
```

## Arguments

- config:

  A simulation config with `reported_cases`, `reported_deaths` and
  `date_start`.

- control:

  A control list; `control$likelihood` may carry `nb_k_cases`,
  `nb_k_deaths` and `nb_dispersion_shrink`.

- score_window:

  Optional resolved scored window (`idx_cases`, `idx_deaths`); the
  dispersion is estimated on the same window the likelihood scores.

## Value

A list with `cases`, `deaths` (each `k` plus a `summary` string) and the
combined `table`.

## Details

The override (`control$likelihood$nb_k_cases` / `nb_k_deaths`)
*replaces* the estimate rather than bounding it, and says so in the log.
This is deliberate: the retired `nb_k_min_*` floor silently overrode a
data-driven estimate, which is the behaviour this design removes.
