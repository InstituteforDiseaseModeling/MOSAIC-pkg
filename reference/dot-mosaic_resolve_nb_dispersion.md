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

A list with `cases`, `deaths` (each `k`, `week_offset` – the
reporting-week boundary per location, which the weekly cases likelihood
uses for its blocks – a `summary` string and `tier_used`, whether
`config$reported_tier` restricted that channel's fit to observed weeks:
`FALSE` for a user-supplied dispersion), the combined `table`, and
`tier_used`, the cases channel's (the dispersion the run log reports for
the cases likelihood).

## Details

The override (`control$likelihood$nb_k_cases` / `nb_k_deaths`)
*replaces* the estimate rather than bounding it, and says so in the log.
This is deliberate: the retired `nb_k_min_*` floor silently overrode a
data-driven estimate, which is the behaviour this design removes.

The estimate uses observed weeks only when the config carries
`reported_tier`
([`est_nb_dispersion`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_nb_dispersion.md),
argument `obs_tier`); without it every week enters the fit, as before
that field existed. A cases location whose fit gives no estimate, or is
clamped at or near the lower bound, takes the shipped panel trend
(`.NB_DISP_PANEL_TREND`, fitted at `burn_in_days = 45`) at every scale.
Deaths take no panel trend: a clamped deaths fit keeps the bound and a
near-bound one keeps its (shrunk) estimate, and both are left out of the
deaths shrinkage-trend fit.
[`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
scores deaths with the reported CFR integrated out, so their NB
dispersion is a diagnostic (and the dispersion of a standalone deaths NB
core). A deaths location whose observed weeks alone are too few is
estimated from every week (`.nb_disp_deaths`), the rule the integrated
deaths likelihood applies to its dispersion.
