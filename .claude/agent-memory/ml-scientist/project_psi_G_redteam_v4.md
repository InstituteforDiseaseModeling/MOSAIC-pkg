---
name: psi-G-redteam-v4
description: Adversarial review of psi "G" config (config_default v4.0) — bias-correction lm corrupts several countries' psi; §8.2 amplitude guard does not exist. B1+B5 FIXED 2026-06-19 (not yet versioned/committed by parent)
metadata:
  type: project
---

Adversarial red-team of psi "G" config (adopted as config_default v4.0), reviewed 2026-06-19
at MOSAIC HEAD v0.44.17. G run on hedgehog: `~/MOSAIC/output/psi_G_production/`
(config.json + ~/psi_G_prod.log); load-bearing output now at
`MOSAIC-pkg/model/input/pred_psi_suitability_day.csv` (`psi` col = `pred_bias_corrected`).

**Why:** G = D-per-capita target + AI data + CW ON + rw_subsample=5 + fit_date_start=2010 +
n_seeds=10, lstm_v2_hierarchical_film / snf_k5 / feature_set v7.3.

**How to apply / findings:**

- BLOCKER: `calibrate_psi_predictions.R` per-country `lm(logit(obs) ~ logit(pred))` on
  outbreak-only weeks is UNREGULARIZED / UNBOUNDED in slope. Degenerate per-country fits
  corrupt psi: GMB collapsed to flat constant 0.0057 (real signal sd 0.059 -> 0.000); ZAF
  blown up 9.7x [0.0001,0.65] from a near-flat predictor (this is the "rank-deficient
  predict.lm" warning); SEN/SWZ gutted to 8%/19% amplitude. These 4 are NOT the
  identity-corrected countries (they have 52-290 outbreak wks so lm ran). Same failure mode
  as [[project_oos_2024_10_suitability_investigation]]. Fix = bound/regularize slope or
  fall back to identity on degenerate fits. `test-calibrate_psi_predictions.R` only checks
  output in (0,1) (always true via plogis) — gives false confidence, no slope guard.

- HIGH: the "§8.2 amplitude guard" referenced in `loss_suitability.R:117-125` and
  `rolling_cv_suitability.R` comments DOES NOT EXIST in code or tests. The stated safety net
  for the CW outbreak-class down-weighting and for bias-correction blowups is unimplemented.

- HIGH: G script set `parallel_seeds=10` PSOCK on all_mosaic pool — contradicts the
  serial-only OOM constraint in [[project_lstm_v2_v034_plan]]. Survived on 448GB hedgehog;
  will OOM near the 32GB target. Non-portable + process-layout-dependent ensemble.

- CV fix (979f066e, skip empty-validation folds) is CORRECT and conservative, but was INERT
  in G (all 10 seeds had all 20 folds populated, no NA best_epochs). `min(50,epochs)`
  fallback floor is sound. No epoch-selection bias in G.

- rw_subsample=5 tiling claim VERIFIED: step=1mo, test=5mo, ss5 => contiguous zero-overlap
  tiling, full coverage. ss2 = 2.5x redundancy (author said 2.4x, immaterial). Tiling fine
  for epoch-selection purpose; best_epochs tight (12-20).

- fit_date_start=2010: early (<=2014, 80-98% AI/synthetic) raw psi has HIGHER mean (0.117)
  and variance (sd 0.22) than real-data era (0.052/0.12) — plausible learned-artifact
  signature. RW-CV test windows start 2018-06, so ALL pre-2018 psi is unvalidated /
  extrapolative. AI-reconstruction leakage (did generator see cases?) is an etl hand-off.

- Logit-scale ensemble averaging CORRECT (`ensemble_suitability.R:279-289`). 10 seeds
  diverge (val_loss 0.371-0.386, epochs 14-20); 10 is reasonable.

- Config JSON matches the claimed G config exactly, BUT does not persist `confidence_weight`
  or `rw_subsample` as explicit fields (only inferable from log) — provenance gap.

Run used MOSAIC 0.44.7 + hot-patched psi_cv_fix.R via assignInNamespace (script asserts patch
took) — artifact not from a tagged release.

**FIX (2026-06-19, parent to version/commit):** B1 + B5 resolved.
- B1: `calibrate_psi_predictions()` now guards each per-country affine — degeneracy screen
  (rank-deficient / `sd(logit_pred) < min_pred_sd=0.05` / <2 distinct preds -> identity),
  slope clamped to `slope_range=c(0.25,4)`, offset to `offset_range=c(-4,4)`, and an
  amplitude clamp that shrinks the affine toward identity (grid blend) so corrected logit-sd
  stays in `amp_range=c(0.5,2)` x input logit-sd. Attaches `attr(.,"calibration_diagnostics")`
  (per-iso status fit/guarded/identity, slope, offset, amp_ratio). Verified on synthetic
  reproductions: ZAF flat-predictor -> identity (was 2.79x blow-up); GMB/SEN/SWZ -> guarded at
  0.50x floor (was 0.02/0.12); COD/SOM/MOZ unchanged (1.30x, status=fit, guard no-op).
- B5: NEW `R/check_psi_amplitude.R` (`check_psi_amplitude()`) — independent post-hoc guard,
  flags collapsed (logit-sd < collapse_abs=0.02 OR ratio<amp_range[1]) / inflated
  (ratio>amp_range[2]); warns + returns per-iso diag. Wired non-fatally at end of
  calibrate_psi_predictions (`attr(.,"amplitude_diagnostics")`).
- Tests in `test-calibrate_psi_predictions.R` (now 27 PASS): ZAF identity-fallback,
  GMB bounded-no-collapse, COD unchanged, check_psi_amplitude collapsed/inflated/ok.
- Did NOT touch est_suitability.R / rolling_cv / loss (other agents own); the §8.2 COMMENT in
  loss_suitability.R is now backed by a real function but the comment text itself was left as-is.

**CONFIRMED ACTIVE @ v0.55.11 (2026-06-26, forecast_cv_smoke prefit, n_seeds=3, cutoff 2025-06-01):**
B1+B5 guards are firing as designed in the frozen psi cache
(`claude/forecast_cv_smoke/psi_cache/psi_2025-06-01.csv`). Log warnings: 15 identity-corrected,
7 affine-guarded, check_psi_amplitude flags 5 inflated (CAF/NER/RWA/UGA/ZMB all at amp_ratio~2.0
= the amp_range[2] CEILING, i.e. CLAMPED not runaway — the pre-fix 9.7x blow-up is gone). BUT the
corruption is mitigated, NOT eliminated: GMB (max=0.019 sd=0.0009) and SEN (max=0.014 sd=0.0004)
are still clamped-flat at the floor; GIN/GNB/SLE collapsed (max<0.05); NER/ZMB/AGO still hit
psi=1.000. So the guard prevents catastrophe but a dozen West/coastal ISOs have psi that is flat
or rail-pinned and should not be trusted as signal. The 3 forecast-cv production targets
(NGA/MOZ/ETH) are CLEAN: well-distributed (sd 0.18-0.26), not rail-pinned, bias-correction is a
real non-identity transform (MOZ psi != pred_smooth on all rows), none appear in any warning list.
Verdict for the NGA/MOZ/ETH 3-country run: GREEN on the psi side — global-fit distortion of other
countries does not feed the target ISOs. Follow-up (non-blocking): the floor-clamp still yields
unusable flat psi for the GMB/SEN/GIN cluster; revisit whether these should fall to a climatology
seasonal prior rather than a flat constant.
