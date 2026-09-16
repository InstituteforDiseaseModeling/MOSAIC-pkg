---
name: forecast-cv-leakage-redteam
description: Red-team of plan_forecast_cv/PLAN.md (2026-06-26) — two structural psi leakage/horizon traps NOT caught by fit_date_stop; the plan over-states "leakage mechanics already correct"
metadata:
  type: project
---

Red-team of `MOSAIC-pkg/claude/plan_forecast_cv/PLAN.md` (forecast-CV suite) from the psi/ML lens,
2026-06-26 at HEAD. The plan's claim "leakage-critical mechanics already correct" is HALF-TRUE:
the date-split/scaler/target-anchor/calibration-window controls ARE clean, but two leaks sit
UPSTREAM of the `fit_date_stop=T` harness mask and are unaddressed.

**Why:** these are durable leakage/horizon traps for ANY psi forecast-CV work, not just this plan.

**How to apply / findings:**

- CRITICAL C1 — OOS covariates are REALIZED climate/ENSO, not cutoff-dated forecasts. Training
  is clean (scaler `build_suitability_sequences.R:288-302`, cases_99th `:221`, RW train rows ≤T),
  but PREDICTION features over the post-cutoff window are whatever today's CSV holds = observed
  reanalysis/ENSO. `compile_suitability_data.R:220-223` DROPS the ENSO `data_source` provenance
  (mixes observed+forecast, no surviving flag). So harness psi is a HINDCAST conditioned on
  observed weather, not a forecast — perfect-foresight covariates inflate OOS skill. Fix: per-cutoff
  pre-fit must consume cutoff-dated forecast-mode covariates (compile_suitability_data has
  `forecast_mode`/`forecast_horizon` args, default 3mo), OR label every result hindcast/skill-ceiling.
  NOT in the plan's gap table at all. This is the opposite of G6 (dates WITHIN coverage that
  shouldn't be observed yet).

- CRITICAL C2 — single global psi fit per cutoff (D2) means per-country OOS cells share a common
  psi generator (shared FiLM trunk/region-embed/partial-pool, all ≤T so not a temporal leak, but
  NOT independent). Undermines D1's "isolated per-country" framing + the block bootstrap (G8).
  Fix: keep global fit (right modeling choice), but FREEZE+HASH the ISO pool in psi_manifest
  (pool change → invalidate cache), block bootstrap on cutoff×psi-fit, drop "isolated" language.

- MAJOR M2 — lstm_v2 production path has the flat-fill end-of-series trap UNGUARDED. TWO fill
  layers: `.psi_weekly_to_daily_smooth` na.locf fwd+back to pred_date_stop (`ensemble_suitability.R:67-68`)
  AND `.rolling_cv_psi_matrix` na.locf fwd+back across config grid (`run_rolling_cv.R:377-380`).
  `.drop_filled_prediction_tail` (`est_suitability.R:14-21`) is wired ONLY into the legacy path
  (zero refs in run_rolling_cv_suitability/ensemble/rolling_cv/build_suitability). Plan 1a/3b assume
  a `last_genuine_date` provenance hook from that helper that DOES NOT EXIST on lstm_v2.
  psi forecast-horizon ceiling = min(ENSO/climate forecast lead ~3-6mo, per-iso last-genuine-covariate
  date). EXCLUDE flat-filled cells (default), don't just flag; cap horizons_months at that ceiling.
  Matches [[project_psi_trailing_fill_endofseries_drop]].

- MAJOR M1 — freezing psi gives bit-reproducibility of an ARBITRARY draw. Fit itself is
  cross-process non-deterministic (recurrent_dropout, see
  [[project_est_suitability_cross_process_nonreproducible]]); n_seeds=5 logit-median
  (`ensemble_suitability.R:286-289`) only modestly stabilizes. Pre-fit should pool MORE seeds
  (≥10), record host+TF version in manifest, add a 2-disjoint-seed-set stability probe per pilot
  country. Do NOT present same-host hash as cross-host reproducibility.

- Confirmed CLEAN (don't re-litigate): fit_date_stop training mask, train-only scaler/cases_99th,
  per-country calibration on date<=fit_date_stop (`calibrate_psi_predictions.R:18,62`, bounded
  post-[[psi-G-redteam-v4]]), logit-scale seed averaging, freeze-once efficiency (N_cutoffs fits).
