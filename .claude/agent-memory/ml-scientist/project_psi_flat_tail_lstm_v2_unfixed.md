---
name: psi-flat-tail-lstm-v2-unfixed
description: v0.44.14 .drop_filled_prediction_tail fix is legacy-path ONLY; lstm_v2 (production) still emits a flat na.locf carry-forward psi tail past the covariate horizon
metadata:
  type: project
---

The v0.44.14 "drop trailing carry-forward fill" fix is **NOT wired into the production
lstm_v2_hierarchical_film path** — it lives only in `.est_suitability_legacy` (`R/est_suitability.R`
~L1315-1329, calls `.drop_filled_prediction_tail`). The lstm_v2 writer in
`R/run_rolling_cv_suitability.R::.est_suitability_lstm_v2` (~L325-348) filters `out_daily` only by
`pred_date_start..pred_date_stop` and never calls `.drop_filled_prediction_tail`.

**Flat-tail mechanism (lstm_v2):** LSTM produces weekly preds only where covariates exist
(`data_bundle$dates_pred`, bounded by the ENSO/CMIP6 horizon). The daily grid in
`R/ensemble_suitability.R::.psi_weekly_to_daily_smooth` (L69-73) runs `start..pred_end` and
`zoo::na.locf` forward-fills (then `fromLast`) — so every day past the covariate edge gets the last
genuine weekly value → constant `pred_raw`. `pred_smooth`/`psi` still micro-wiggle (LOESS +
bias-correct on the flat raw), masking it on a quick look; check `pred_raw` for the dead-flat run.

**Symptom seen (NMME full_metapop, 2026-06-26 artifact):** run used pred_date_stop=2027-02-04,
fit_date_stop=2025-06-01 (per `model/input/psi_suitability_config.json`). Covariate edge =
**2026-10-29** (last genuine weekly date, confirmed in `model/input/data_psi_suitability.csv`).
29/40 countries go flat from exactly 2026-10-29 → 2027-02-04 = **99 days (~3.3 mo) flat fill**.
(Countries flattening earlier at value 0.0100 are the per-country floor, unrelated.)

**Not a 5-month-cap bug.** There is no hard ~5-month horizon cap; the only hardcoded horizon is the
`.psi_build_data` default `pred_date_stop = cutoff+6 months` (build_suitability_sequences.R L170),
which is OVERRIDDEN here. The horizon ceiling = covariate (ENSO/CMIP6) coverage, exactly as the
est-suitability skill states. User's "aiming at 5-mo horizon" hypothesis is wrong in mechanism but
right in effect (flat tail). NMME retrofit didn't regress the fix — the fix was never in the v2 path.

**Fix options (proposed, not yet applied):** (A) port `.drop_filled_prediction_tail` into the v2
writer — capture last genuine `dates_pred` per iso BEFORE the daily na.locf, drop rows beyond it;
matches legacy + relies on make_config_default truncating the sim window to common coverage. (B) set
pred_date_stop to the covariate edge so no fill happens. (A) is the consistent fix. Coordinate with
downstream make_config_default truncation (swe owns the config/prediction-fill side).

See also [[forecast-cv-leakage-redteam]] (already flagged "lstm_v2 flat-fill tail UNGUARDED").
