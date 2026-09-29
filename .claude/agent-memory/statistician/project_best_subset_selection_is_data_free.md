---
name: best-subset-selection-is-data-free
description: The BFRS best-subset rule selects |B| = 1.15 x ESS_best independent of n and of the likelihood; ESS_B/A/CVw are closed-form in |B|; the convergence gate is tautological. Measured @ v0.90.3 on two production runs.
metadata:
  type: project
---

The best-subset selection rule contains **no free response to the data**. Verified @ v0.90.3 on
ETH 25,000 draws (local `/Users/johngiles/MOSAIC/output/eth25k_v0903/`) and 40-loc 100,000
draws (`dugong:~/prod100k_v087/`).

**Why:** both runs wrote *bit-identical* `subset_selection_summary.csv` metrics
(`optimal_size=115`, `ESS_B=107.475080287883`, `A=0.985737810971224`, `CVw=0.561965420254235`)
despite differing 4x in draws, 40x in locations, and **25x in the dAIC of the worst retained
member** (4,215 vs 104,525). Two datasets cannot agree to 12 digits unless the metric is blind
to the data.

**The invariants (all reproducible with no data):**

1. **`|B| = 1.15 x control$targets$ESS_best`.** Measured 59/115/230/341 for targets
   50/100/200/300. Independent of `n_total`: |B| = 115 at n = 500, 1k, 2.5k, 5k, 10k, 25k.
   It is a fixed COUNT, not a fixed percentile (percentile fell 23% -> 0.46%).
2. **`ESS_B`, `A`, `CVw` are closed-form in |B|.** Once `Delta_(2) > 4` (always: measured 489
   ETH / 14,078 100k), `pmin(delta,4)` = `c(0, rep(4, n-1))` exactly, so
   `w = (1, e^-2, ..., e^-2)/Z`. At n=115 this gives perplexity ESS 107.475, **Kish ESS 87.4**
   (the 87.4 in [[project_weighting_stack_measured_v085]] is the same number, Kish not
   perplexity — no conflict).
3. **`ESS(n)/n ~ 0.87` at every n**, because `grid_search_best_subset()` sets
   `eta = 0.5*(4/range(delta))` — the weights are **exactly scale-invariant in the loss**.
   So the stopping rule `ESS(n) >= 100` fires at n ~ 100/0.87 ~ 115 on any dataset.
4. **`A >= 0.70` is mathematically UNREACHABLE.** Weights confined to `[e^-2, 1]` give
   `A_min` = 0.86 (n=30) to 0.93 (n=1000). `CVw_max` = 1.175 vs target 1.0 — can bind only in
   an extremal config never realised. So only ESS can ever bind, and **tiers 1-20 of
   `get_default_subset_tiers()` are inert** (they relax only A and CVw): 19,420 wasted grid evals.
5. **The convergence gate is tautological.** `ESS_B(saturated) >= ESS(grid-search)` at every
   n >= 50, and the grid search guarantees `ESS(grid-search) >= T` before returning. The gate
   cannot fail when consulted.

**TWO different weighting schemes, one undocumented.** Selection uses `eta = 2/range(delta)`
(`grid_search_best_subset.R:147`, `optimize_ensemble_subset.R:309`); production writes
`weight_best` from `pmin(delta,4)`, `eta=0.5` (`run_MOSAIC.R:1869, 1988`). Only the latter is in
`05-model-calibration.Rmd`. So |B| is chosen under one estimator and used under another.

**How to apply:** never read `ESS_B`/`A`/`CVw` as evidence about fit or about sampler coverage —
they are functions of `ESS_best` alone. Read `ess_is_all` / `khat_all` from `summary.json`
instead (`calc_is_diagnostics()`, computed but NOT gated). Before attributing any run-to-run
difference to the subset rule, check whether `|B|` moved — if `ESS_best` is unchanged it did not.

Full report: `/Users/johngiles/MOSAIC/MOSAIC-pkg/claude/review_inference/findings/SUBSET.md`;
scripts `.../review_inference/scratch/SUBSET/`. See also
[[project_subset_weights_do_no_work]] and [[project_weighting_stack_measured_v085]].
