---
name: optimize-subset-levers
description: optimize_ensemble_subset() speedups (loop-reorder, PSOCK cell-split, stride-refine) + the bit-identical scatter contract that the Tier-2 parity test guards
metadata:
  type: project
---

`optimize_ensemble_subset()` (R/optimize_ensemble_subset.R) gained three opt-in
speed levers (~v0.44.6), all approved by the package author.

**Why:** the post-calibration "Optimizing ensemble subset" step ran a fully
sequential nested R loop, cost quadratic in best-subset size, on the main process.

**How to apply / what landed:**
- Lever A (loop reorder): one cell-outer kernel `.optimize_eval_cell_block()`
  gathers `xc/xd` ONCE per [loc,time] cell, then loops the N grid. Replaces the
  old N-outer loop that re-gathered per N.
- Lever B (`cl=`): PSOCK cell-split via `parallel::splitIndices` + `parLapply`.
  Heavy objects (cases/deaths arrays, ord_c/ord_d/pid_full) are `clusterExport`ed
  ONCE as `.GLOBAL_*`; workers call `MOSAIC:::.optimize_eval_cell_block` and must
  NOT close over the `ensemble`/optimize frame. FORK forbidden (reticulate/numba
  native threads loaded post-calibration -> fork-deadlock).
- Lever C (`stride=`): stride>1 = coarse grid then refine bracket; eval_table then
  holds only evaluated N's (fewer rows) and may pick a different optimal_n. stride=1
  is the exhaustive parity path.

**Bit-identical scatter contract (the load-bearing trap):** cell index
`ci=(i-1)*n_times+j` (i outer, j inner) matches `.presort_cell_orders`. The kernel
returns per-cell vectors in `ci` order; `.scatter_cells()` rebuilds the [n_locs x
n_times] matrix with `matrix(v, n_locs, n_times, byrow=TRUE)` so `v[ci]` lands at
[i,j], then `as.numeric()` flattens column-major exactly as the historical loop did.
Getting byrow wrong silently breaks parity. tests/testthat/test-tier2_parity.R
asserts the default path (stride=1, cl=NULL) bit-identical at tolerance=0 — run it
after any touch.

**run_MOSAIC wiring:** two control keys in `default_predictions` —
`optimize_stride=1L`, `optimize_n_cores=1L`. The optimize block builds its OWN
short-lived PSOCK cluster (calibration cluster already stopped by then) only when
n_cores>1 AND n_cells>=2, and stopCluster's it (on.exit + explicit).

**Testing gotcha:** the parallel bit-identical test needs the INSTALLED package to
carry `.optimize_eval_cell_block` (workers do `library(MOSAIC)` + `MOSAIC:::`).
Under `devtools::test()`/load_all, R CMD INSTALL the package first or the worker
resolves a stale install. Test skips if the installed namespace lacks the internal.

See [[hedgehog-run-infra]] for where these parallel runs actually execute.
