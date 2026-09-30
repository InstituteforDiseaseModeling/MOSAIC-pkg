---
name: ensemble-artifact-slimming
description: run_MOSAIC persists slimmed ensemble RDS (arrays stripped) by default via control$io$persist_ensemble_arrays; FOUR save sites, not three
metadata:
  type: project
---

Ensemble artifact slimming shipped v0.60.0.

`run_MOSAIC()` strips the dense 4-D `cases_array`/`deaths_array` from the persisted ensemble RDS
files by default (control$io$persist_ensemble_arrays = FALSE). In-memory object is NEVER stripped —
only the copy handed to each saveRDS.

**Why:** MOSAIC-results moved to text-only git backend; slimmed ensemble (~tens of KB) travels in git;
OCV + rolling-CV + plotting only ever read light fields (verified: zero cases_array/deaths_array refs
in plot_model_ensemble.R, run_rolling_cv.R, evaluate_rolling_cv.R).

**How to apply:**
- Strip helper `.mosaic_ensemble_drop_arrays(ens)` lives near other `.mosaic_ensemble_*` in
  R/calc_model_ensemble.R; nulls both arrays, preserves S3 class + light fields, idempotent.
- FOUR heavy save sites in run_MOSAIC.R (handoff said three — it MISSED subset_opt.rds):
  1. ensemble_candidate.rds
  2. ensemble_optimized.rds (`subset_opt$ensemble_optimized`)
  3. subset_opt.rds — re-embeds arrays via $ensemble_optimized; strip a COPY (subset_opt_to_save),
     never mutate the in-memory subset_opt used later
  4. medoid_ensemble.rds
- `.persist_arrays <- isTRUE(control$io$persist_ensemble_arrays)` defined once alongside
  central_method (~L2420).
- Fallback (CORRECTION 2): after the optimize block, if ensemble_optimized.rds was never written
  (optimize off or empty subset) write the candidate ensemble there — OCV hardcodes that filename.
- Guard (CORRECTION 3): add_reproductive_numbers.R `.add_reff_recompute_ci()` reads
  ensemble_candidate.rds then does `dim(ens$cases_array)[2L]` — on a slimmed file that's dim(NULL).
  Early stop() right after readRDS if is.null(ens$cases_array), pointing to persist_ensemble_arrays=TRUE
  or the trajectories CI path. This fn is post-hoc/exported, NOT called from run_MOSAIC.

run_MOSAIC.R has ZERO readRDS calls; every post-save consumer (optimize_ensemble_subset, medoid
block, trajectory reduce, implied-CFR) reads the LIVE in-memory object, so slimming the saved copies
cannot affect them. Related: [[project_trajectories_integration_plan]] (trajectories_ensemble.rds is
the separate artifact the R_eff CI path normally uses).
