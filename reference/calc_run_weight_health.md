# Weight health and convergence status of a finished calibration, from its output directory

Recomputes
[`calc_weight_health`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_weight_health.md)
(and so the 1.4.0
[`calc_convergence_status`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_convergence_status.md))
for an existing
[`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
output directory from its stored likelihoods, so runs produced before
the diagnostic existed (MOSAIC 1.1.x, 1.3.0) can be assessed without
re-running anything. It reads `2_calibration/samples.parquet`,
`1_inputs/config.json` and `1_inputs/control.json` (and
`3_results/summary.json` when present, for the location label). It never
modifies those files.

## Usage

``` r
calc_run_weight_health(
  dir_output,
  write = TRUE,
  coarsening_alpha = NULL,
  weight_col = NULL,
  n_batches = 10L,
  best_subset_ratio = NULL,
  ess_best_target = NULL,
  min_members = NULL,
  max_members = NULL,
  thresholds = NULL
)
```

## Arguments

- dir_output:

  A
  [`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
  output directory.

- write:

  Logical; write `3_results/weight_health.json`. Default `TRUE`.

- coarsening_alpha:

  Effective number of independent observations of the posterior. Default
  `NULL` means the run's `control$targets$coarsening_alpha`, or the
  package default for a run that predates it.

- weight_col:

  Deprecated since MOSAIC 1.4.0 and ignored (with a warning): the status
  is assessed on the best-fit members.

- n_batches:

  Number of seed batches. Default `10`.

- best_subset_ratio, ess_best_target, min_members, max_members:

  Optional overrides of the best-fit settings (see Details).

- thresholds:

  Optional named list overriding the status bands.

## Value

Invisibly, a list with `location`, `dir_output`, `timestamp`, `fields`
(the flat summary.json schema 2 fields) and `weight_health` (the full
[`calc_weight_health()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_weight_health.md)
result).

## Details

`n_obs` is the number of scored location-day cells with a finite cases
or deaths observation inside the scored window, after the burn-in and
the deaths-era start of `control$likelihood`.

The best-fit settings come from the run's control when it is a 1.4.0
control (it carries `targets$status_thresholds`):
`targets$best_subset_ratio`, `targets$ESS_best`,
`targets$min_ensemble_members`, `targets$max_ensemble_members` and
`targets$status_thresholds`. An older control is assessed on the 1.4.0
defaults, because its `ESS_best` and member caps meant something else.
Arguments override both. A run is fixed-budget when its control has a
positive `calibration$n_simulations`; an adaptive run has exhausted its
budget when its draws reach `calibration$max_simulations_total`. The
growth slope is replayed over the draws in simulation order
(`.mosaic_ess_replay()`, as for a fixed-budget run), so a stalled growth
is flagged. Simulations attempted are the rows of `samples.parquet`.

## See also

[`calc_weight_health`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_weight_health.md),
[`calc_convergence_status`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_convergence_status.md)

Other calibration-metrics:
[`calc_convergence_diagnostics()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_convergence_diagnostics.md),
[`calc_convergence_status()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_convergence_status.md),
[`calc_model_agreement_index()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_agreement_index.md),
[`calc_model_cvw()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_cvw.md),
[`calc_model_ess()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_ess.md),
[`calc_weight_health()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_weight_health.md)

## Examples

``` r
if (FALSE) { # \dontrun{
wh <- calc_run_weight_health("output/national/MOZ/v2026-10-08.01")
wh$fields$convergence_status
wh$fields$convergence_reasons
} # }
```
