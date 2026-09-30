---
name: hazard-gam-train-stop-leakage-hook
description: v0.62.1 Path-A leakage hook — gam_train_stop on all 3 hazard imputers; the NON-OBVIOUS part is that fitting <=stop alone is NOT enough, the sentinel country/global-mean fill ALSO leaks future rows
metadata:
  type: project
---

The [[v7-4-hazard-covariates-built]] hazard GAMs (flood/cyclone/drought) fit on
the FULL series, which leaks in rolling-origin forecast-CV (psi LSTM fit
<=cutoff but its hazard-covariate INPUTS were imputed on all data; ml-scientist
measured ~0.05-0.07 OOS contamination). Fixed at MOSAIC v0.62.1 (Path-A durable
hook, NOT committed).

**The hook:** `gam_train_stop` arg (Date/char/NULL, default NULL = back-compat
full-data) on `impute_flood_probability`, `impute_cyclone_probability`,
`impute_drought_probability`; passthrough on `compile_suitability_data(...,
gam_train_stop=NULL)`. When set: GAM FIT on rows `date <= gam_train_stop`,
PREDICT all rows. Drought SPEI-threshold LABEL rolling-mean still uses all rows
(input transform); only the FIT is capped. select=TRUE/fREML/formulae unchanged.

**NON-OBVIOUS GOTCHA (caught by the leakage regression test, not by
inspection):** capping the FIT alone does NOT make <=stop predictions
leak-free. The sentinel NA-fill (`country_mean <- tapply(preds, iso, mean)` then
global mean) is computed over ALL predicted rows including >stop, so any <=stop
row that hit the sentinel (lag warm-up edges, ~first 24w cyclone / 26w drought
integrator per iso) gets filled with a mean informed by future rows.
**Fix:** when gam_train_stop is set, all sentinel means (point-prob country +
global, AND the drought integrator warm-up country + global) are masked to
`date <= gam_train_stop`. This is what makes the leakage test pass
(perturb/flip-label/truncate >stop rows -> <=stop preds byte-identical via
expect_identical). Test: tests/testthat/test-impute_gam_train_stop.R.

**source_csv override (v0.62.1):** `est_suitability(..., source_csv=NULL)` +
`.est_suitability_lstm_v2(..., source_csv=NULL)` (run_rolling_cv_suitability.R)
— default NULL falls back to canonical
`file.path(PATHS$DATA_CHOLERA_WEEKLY, "cholera_country_weekly_suitability_data.csv")`;
a path override points psi fitting at an arbitrary panel (v7.4-tagged /
per-cutoff leak-free) without renaming files. lstm_v2 ONLY (legacy path errors
if source_csv supplied — it's frozen). MODEL_INPUT check kept; DATA_CHOLERA_WEEKLY
check now only required when source_csv is NULL. FLAG for swe/ml (shared-file).

**De-duplication:** ml's `claude/psi_v74_screening/build_leakfree_hazard_panels.R`
re-implemented the cyclone+drought GAM formulae verbatim to fit <=cutoff.
Replaced with calls to the cutoff-aware imputers (gam_train_stop=cutoff) — ONE
source of truth per formula. Refactored builder reproduces ml's GENUINE preds
bit-for-bit (max|diff|=0 on all non-sentinel rows); differs ONLY on the panel's
2009 leading-edge warm-up placeholders (cyclone 24 rows/iso, drought 49
rows/iso) where ml's OLD sentinel leaked — that divergence IS the fix, ~9y
before any cutoff, never scored. Builder leaves FLOOD baked (matches ml's
scope); flood imputer still got the arg for completeness.

**NOT wired** into prefit_rolling_cv_psi / run_rolling_cv (swe's critical-path
lane, v2 re-run). This is just the durable hooks + de-dup.
