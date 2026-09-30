---
name: rundir-control-json-wrapper-trap
description: 1_inputs/control.json nests the control under $control; render_MOSAIC_figures() reads ctrl$predictions (always NULL -> median) so every render of a v0.98.0+ mean run plots the MEDIAN; plus the Python-era final-deaths mask that the R engine does not need
metadata:
  type: project
---

`run_MOSAIC()` writes `1_inputs/control.json` as `control_record = list(control = control, n_iterations, iso_code, timestamp, paths)`. Any post-hoc reader must index `ctrl$control$...`. The resume checker does (`persisted_ctrl$control`); `render_MOSAIC_figures()`'s `.resolve_central()` read `ctrl$predictions$central_method`, which is always NULL, so it fell back to "median". The bug was latent on main while the default was median. v0.98.0 flipped the default to "mean", which made it live: the ensemble and medoid PDFs caption "Central: median" and show summary.json's `*_median` R2/bias, while CSVs, trajectories and the windows figure are mean. Found in the 2026-09-29 CFR v2.1 pre-merge plotting audit; unfixed at that time.

**Why:** the render test fixture wrote the UNWRAPPED shape `list(predictions = list(central_method = "median"))`, using the fallback value, so it could never fail. This is the CLAUDE.md lesson #12(iii)/#13 shape: a fixture that mocks the schema false-passes.

**How to apply:**
- Any code reading a run directory's control.json should use `$control$...`.
- Fixtures must mirror `control_record`, and must use a NON-default value so the read is actually exercised.
- When a default flips, grep every consumer that re-derives it from disk.

Related trap from the same audit: `mask_final_deaths_step = TRUE` blanks the final deaths cell citing laser-cholera #82 ("structural zero, reproduced by the R engine"). That is false for the R engine: `sim_results.R` gathers `reported_cases` and `reported_deaths` with the same trim-last rows, and the CFR v2.1 post-hoc redraw writes the last column. Measured final-day deaths were non-zero. Don't cite #82 for new masking. See [[cfr-v21-engine-review]].
