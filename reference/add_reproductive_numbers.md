# Add Cori R_eff to an existing MOSAIC model output directory

Post-hoc driver that applies the Cori (2013) effective reproductive
number reduction
([`calc_Reff`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_Reff.md))
to a **single** MOSAIC model output directory laid out by
[`run_MOSAIC`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md).
It loads the captured ensemble trajectories
(`2_calibration/trajectories_ensemble.rds`) and the medoid config
(`1_inputs/config.json`), computes the per-location R_eff series, writes
the tidy `reproductive_numbers` table to
`3_results/posterior/reproductive_numbers.csv` (and `.rds`), and
optionally renders
[`plot_Reff`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_Reff.md)
to `3_results/figures/reproductive_number/`.

## Usage

``` r
add_reproductive_numbers(
  output_dir,
  recompute_ci = FALSE,
  burn_in_days = NULL,
  infectiousness_floor = 1,
  plots = TRUE,
  overwrite = TRUE,
  verbose = TRUE
)
```

## Arguments

- output_dir:

  Character. Path to ONE MOSAIC model output directory (the directory
  that contains `1_inputs/`, `2_calibration/`, and `3_results/`). No
  path is hardcoded; all I/O is relative to this.

- recompute_ci:

  Logical. When `TRUE`, build a proper posterior credible interval by
  **re-simulating** the saved posterior ensemble
  (`2_calibration/ensemble_candidate.rds`) and computing R_eff per
  member, then weighted quantiles (median + 95\\ S-\>E infection
  `incidence` channel that the persisted trajectory artifact does not
  retain at a daily grid. A faithfulness gate confirms the re-sim
  reproduces the saved `cases_array` before any CI is written. When
  `FALSE` (default) the cheap point-estimate path is used (renewal on
  the medoid weighted-median incidence from `trajectories_ensemble.rds`;
  CI columns are populated only if the artifact carries
  daily-consecutive per-member lines, otherwise NA).

- burn_in_days:

  Integer or `NULL`. Number of leading days to exclude from the R_eff
  output (set to `NA` in the table). `NULL` (default) reads
  `control$likelihood$burn_in_days` from `1_inputs/control.json`; if
  that is `0` or absent it defaults to `30` days. Only consumed on the
  `recompute_ci = TRUE` path.

- infectiousness_floor:

  Numeric scalar \\\ge 0\\. Passed to
  [`calc_Reff`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_Reff.md);
  minimum generation-weighted past infectiousness required to report
  \\R_t\\ (guards the initial-condition seed spike and deep
  inter-epidemic troughs). Default `1`.

- plots:

  Logical. Render and save
  [`plot_Reff`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_Reff.md)
  (PNG + PDF) to `3_results/figures/reproductive_number/`. Default
  `TRUE`.

- overwrite:

  Logical. If `FALSE` and the output CSV already exists, skip
  recomputation and return a `"skipped_exists"` status. Default `TRUE`.

- verbose:

  Logical. Emit progress messages. Default `TRUE`.

## Value

Invisibly, a one-row `data.frame` status with columns:

- dir:

  The `output_dir`.

- status:

  One of `"ok"`, `"skipped_exists"`, `"skipped_missing_trajectories"`,
  `"skipped_missing_config"`, `"skipped_no_incidence"`, or `"error"`.

- n_locations:

  Number of locations in the artifact (`NA` if not loaded).

- ci_available:

  Logical; whether the posterior CI columns are populated (`FALSE` on
  production-default strided artifacts).

- csv:

  Path to the written CSV (`NA` when not written).

- rds:

  Path to the written RDS (`NA` when not written).

- plot:

  Path to the written PNG (`NA` when not written).

- message:

  Human-readable detail.

## Details

**Robust by design.** Missing files, a trajectory artifact without the
`incidence` channel, or a
[`calc_Reff()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_Reff.md)
error are handled by skipping with an informative message/warning and
returning a status row; the function does not crash. It is therefore
safe to map over the full per-ISO model tree.

## See also

[`calc_Reff`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_Reff.md),
[`plot_Reff`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_Reff.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Single model directory
add_reproductive_numbers("/path/to/output/national/MOZ")

# Map over a per-ISO tree
dirs <- list.dirs("/path/to/output/national", recursive = FALSE)
status <- do.call(rbind, lapply(dirs, add_reproductive_numbers))
} # }
```
