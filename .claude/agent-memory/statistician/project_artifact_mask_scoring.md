---
name: artifact-mask-scoring
description: Metrics-only engine-artifact mask excludes cases warm-up + final deaths-zero from R2/bias scoring (v0.46.0)
metadata:
  type: project
---

Post-calibration R²/bias scoring in `run_MOSAIC()` now excludes two laser-cholera
(0.15.0) array artifacts from the EST central series, via `artifact_mask` carried on
the `mosaic_ensemble` object and applied at scoring sites by
`.mosaic_mask_central_for_scoring(mat, chan, spec)` (run_MOSAIC_helpers.R, @noRd).

The two artifacts:
- **cases warm-up**: first `cases_warmup` (default 2) cases timesteps = IC transient
  (seeded E flushing into new_symptomatic before SEIR settles). A 1.5× spike at index 1
  in production config_default.
- **deaths final**: last deaths timestep = structural 0 (reported_deaths written at
  [tick] then [1:]-trimmed; laser issue #82). `deaths_final` default TRUE.

**Why:** Before v0.46.0 these positions contaminated R²/bias even though
`plot_model_ensemble()` already masked them for display. Design was LOCKED to
"carry the spec, mask only at scoring sites" — `calc_model_ensemble()` records the
spec (defaults match plot_model_ensemble: `n_cases_warmup_mask=2L`,
`mask_final_deaths_step=TRUE`) but does NOT mutate cases_mean/median, deaths_*,
ci_bounds, or the raw arrays.

**How to apply:**
- Mask invariant: set artifact COLUMNS (=time) to NA on the EST matrix only; OBS stays
  unmasked. `calc_model_R2`/`calc_bias_ratio` are `na_rm=TRUE` and drop NA pairs, so a
  masked est position is excluded pairwise (equivalent to physically dropping that
  column from both series — verified by regression test).
- NULL-spec fallback is `list(cases_warmup=2L, deaths_final=TRUE)` for older/sub
  ensembles lacking the field.
- DO NOT mask the medoid SELECTION target `cen_cases_target` (run_MOSAIC ~L2528) — that
  is a selection distance, not a fit metric. Leave unmasked.
- Windowed metrics reuse the already-masked `cen_c_flat`/`cen_d_flat`, so masking the
  canonical site fixes them too (no double-handling).
- Out of scope (unchanged): CSV/prediction export, optimize_ensemble_subset.R,
  run_rolling_cv.R, plot series (plot keeps its own display mask).
