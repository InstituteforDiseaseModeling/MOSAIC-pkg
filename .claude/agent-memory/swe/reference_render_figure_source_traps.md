---
name: render-figure-source-traps
description: render_MOSAIC_figures / plot_model_ensemble traps -- check each figure's SOURCE (and display vs CSV table), the burn-in display contract (v1.0), and the empty-dir side effect of rendering into a finished run dir
metadata:
  type: reference
---

The renderer is "pure read-render" from the run dir, but a figure can quietly read something
other than the run's posterior. Check where a figure's numbers come from, not just whether
its field names are current; grep what render_MOSAIC_figures.R passes in as well as the plot body.

**Fixed since the v0.99.9 review (verified 2026-10-01 on release/v1.0):** spatial figs 2-4 now
swap posterior medians into the input config (`.mosaic_posterior_mobility_config`); psi_star
reads the run's `config.json$psi_jt` (PATHS only as fallback); ensemble captions (per-location
AND faceted) score via `.mosaic_mask_central_for_scoring()`; `mobility_tau_ci.csv` is written by
run_MOSAIC; subset-opt marker uses `subset_opt$optimal_score`.

**Burn-in display contract (release/v1.0):** `plot_model_ensemble(show_burn_in = TRUE)` (default)
draws from step 1 via `.mosaic_display_prediction_table()` = the CSV assembly with head masks off,
shading the unscored head. The exported `predictions_*.csv` must KEEP the NA head -- the frozen
v1.0 acceptance evaluator relies on it. Never route the display table into
`.mosaic_write_prediction_csvs()`. Golden CSV fixture: tests/testthat/fixtures/predictions_burnin_golden_*.csv.

**Rendering into a finished run dir has a side effect:** `render_MOSAIC_figures()` calls
`.mosaic_ensure_dir_tree()`, which creates every missing tree dir. Production run dirs lack
`2_calibration/samples` (shards removed after combine), so a re-render leaves an empty one
there. Remove it afterwards if it was newly created and the run dir must stay untouched.

**Central method of the in-run render (v0.101.0, red team CM-01):** `.mosaic_run_central_method()`
reads summary.json FIRST, but run_MOSAIC writes summary.json only AFTER the in-run render, so a
re-run into a finished dir drew the earlier run's line. Fixed on fix/v0101-rt-obs (10ca79c1b):
`.mosaic_clear_posterior_artifacts()` removes summary.json, and run_MOSAIC calls the internal
`.mosaic_render_figures(..., central_method =)` (the exported `render_MOSAIC_figures()` is a thin
wrapper; no exported-signature change). When adding a figure reader, prefer an explicit argument
from run_MOSAIC over re-deriving run state from files that may be stale. A new internal render
body must also go in test-no-dangling-globals.R's target list.

**Byte-identity checks of CSVs:** see [[reference-cross-platform-csv-digits]] -- regenerating a
dugong-written CSV on the Mac is NOT byte-identical even with identical code and data.

Related: [[psock-export-and-dead-guard-traps]], [[control-json-wrapper-trap]].
