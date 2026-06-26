---
name: reference-output-tree-canonical-paths
description: Canonical run_MOSAIC() output-tree facts that AI-context files repeatedly get wrong (config_medoid location, no config_best) — verify against R/run_MOSAIC_helpers.R dirs map before trusting any skill/CLAUDE.md claim
metadata:
  type: reference
---

Where post-calibration artifacts ACTUALLY land (verified against source, 2026-06-26).
These are the facts skills/CLAUDE.md keep stating wrong:

- **No `config_best.json`.** `run_MOSAIC()` intentionally produces no best-likelihood model
  (`R/run_MOSAIC.R:2880`). Any file claiming "best model saved to config_best.json" is stale.
  (The previously-flagged stale root `MOSAIC/CLAUDE.md` reference was FIXED in Phase 1 — grep both
  CLAUDE.md files for `config_best` now returns nothing; run-mosaic + forecast-cv state it correctly.)
- **`config_medoid.json` lives in `2_calibration/best_model/`**, NOT `3_results/`. The dir map is
  `dirs$cal_best_model = file.path(dir_output, "2_calibration/best_model")`
  (`R/run_MOSAIC_helpers.R:1052`); written at `R/run_MOSAIC.R:2916`. `R/run_fit_sandbox.R:23` doc
  corroborates the `.../2_calibration/best_model/config_medoid.json` path.
- `3_results/` holds summary.json, predictions/, figures/ — not the medoid config.

**Pattern:** the output-tree ASCII block is a recurring rot source — it's prose that duplicates a
path map that lives in code. Before flagging or trusting one, grep `dirs\$` assignments in
`R/run_MOSAIC_helpers.R`. Prefer skills that say "see the dirs map" over ones that hard-draw the tree.
