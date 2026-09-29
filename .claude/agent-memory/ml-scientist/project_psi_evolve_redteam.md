---
name: psi-evolve-redteam
description: Red-team of psi_evolve phase 1/2/3 — 6 quantified defects incl. refit epoch = best+patience, WIS built on seed dispersion, cp99r target leak (32% of pool weight), dir_acc pinned by LOESS, stride-90 seasonal aliasing, panel rebuild silently changed ENSO/IOD across all history
metadata:
  type: project
---

Adversarial review of `claude/psi_evolve/` (FINAL_REPORT, PLAN_PHASE2, PLAN_ARCHITECTURES,
tft/) on 2026-09-21. Six findings that are NOT visible from reading any single file.

**1. The refit epoch is `best_epoch + patience`, not `best_epoch`.**
`R/loss_suitability.R` sets `n_epochs <- length(history$metrics$loss)` — the number of epochs
RUN under `callback_early_stopping(patience = 10, restore_best_weights = TRUE)`. Keras stops at
`best + patience`, so `round(median(best_epoch))` in `.psi_fit_predict_rw_cv()` refits ~10 epochs
past the validation optimum with early stopping OFF. Reported epochs 16-26 ⇒ true optima 6-16
(up to 2.5x over-trained). Also compresses the *relative* spread between CV geometries by an
additive constant, which partly manufactures the "geometry is inert" result.

**2. Every WIS in the programme is incoherent.** `q025/q25/q75/q975` in `pred_psi_*_day.csv` are
cross-**seed dispersion** quantiles of `pred_smooth` (pre-bias-correction); `psi` is
post-correction. `prefit_rolling_cv_psi()` copies the CSV verbatim. `.rcv_wis` with near-
degenerate intervals collapses to ≈ MAE + a width term ⇒ explains WIS's 28.4% replicate noise vs
MAE's 0.09%, and invalidates T1's (lead=12) rejection. `score_psi_arm.R` documents the defect;
`accuracy_table.R`/`shape_table.R` (which produced the report) reproduce it.

**3. Target leak via period-normalised cp99r, worth 32% of the burden weight.**
`target_D_rate_per_country_floored` divides by a per-country p99 taken over the WHOLE panel
window (post-cutoff included); the LSTM reads that column directly (only the legacy
`intensity` recipe is train-only). Measured at the 2024-01-01 cutoff, train-only vs whole-panel
target scale: RWA 0.154, NGA 0.773, BDI 0.773, SSD 0.868, ZMB 0.930, rest ~1.00. Affected
countries carry 0.316 of the pool weight.

**4. `dir_acc` ≈ 0.5 is largely a smoother artifact, not evidence about covariates.**
psi is LOESS-smoothed on the DAILY grid with `span = 0.025, degree = 2` over a ~4,400-day series
⇒ a ~110-day local quadratic ⇒ at most 1-2 sign changes of `diff(psi)` inside a 13-week block,
while the weekly target flips 5-6 times. Any smooth predictor is pinned near 0.5.
`pred_raw` (un-LOESS'd) is in every psi CSV and has NEVER been shape-scored — only level-scored
(`diag_psi_decay.R`). Phase 2's whole premise rests on the un-tested reading.

**5. Stride 90 aliases the seasonal phase; stride 84 does not.** Measured with
`.psi_make_rw_cv_steps` at cutoff 2024-01-01: F6 (stride 90) puts 23 of 33 fold test_starts in
Mar/Jun/Sep/Dec; F5 (stride 84) is uniform (2-4 per month). `rolling_cv_suitability.R`'s own
HA-01 docstring warns about this for 91 days. Also: F4/F5 already have `step_days == test_days`,
so F6's "tiling is the missing combination" is false — F5 already tiles AND starts early.
Real fold counts (current panel): F5 = 306, F6 = 333 over the 9 cutoffs, not the planned 230/213.

**6. A panel rebuild changes FEATURES, not just targets.** Between
`..._frozen_2026-09-17.csv` and the live panel, `temperature_2m_mean`/`precipitation_sum` are
bit-identical for historical dates (realized reanalysis) but `ENSO3`/`ENSO34`/`IOD` differ across
ALL history (MOZ 2015-03: ENSO34 0.50→0.78, IOD 0.17→0.29). `verify_rebuild.R` diffs only schema,
target and row counts — it never diffs a covariate. So no two panel eras are comparable even on a
shared block, and the teleconnection covariates (the only ones carrying S2S information) are the
least stable in the set.

**Also worth keeping:** the 12-week "forecast" is scored with REALIZED post-cutoff weather, so
`lead = 0` is the *task-matched* objective for what is being measured — the phase-2 inference
"nowcaster ⇒ coin-flip timing" does not follow. Settle the known-future/observed-past split
(NF2) BEFORE running `lead = 12` (T2). And the incumbent FiLM LSTM is ~150k params (vs TFT's
74.8k) on ~320 non-overlapping windows; no arm in any plan REDUCES capacity.

Related: [[project_psi_flat_tail_lstm_v2_unfixed]], [[project_psi_artefact_provenance_v077]],
[[project_forecast_cv_leakage_redteam]], [[project_oos_2024_10_suitability_investigation]].
