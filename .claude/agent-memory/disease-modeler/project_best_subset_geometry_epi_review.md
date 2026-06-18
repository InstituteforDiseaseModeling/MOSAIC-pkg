---
name: best-subset-geometry-epi-review
description: Epi red-team of mosaic_best_subset_presets() — R1 BLOCK (A=0.95 geometry uncertified); R2 rewrite DROPS geometry override -> epi block LIFTED (1 minor CONCERN on subset_size floor)
metadata:
  type: project
---

Red-team of draft `MOSAIC-pkg/claude/draft_mosaic_best_subset_presets.R` (best-subset ensemble
selection preset for run_MOSAIC). Empirical basis: `MOSAIC-Mozambique/output/analysis/
best_subset_metaparams_dataset.csv` (443 runs, ~88% MOZ, observational, 75 cols).

## Round 1 (2026-06-17): BLOCK — v1 shipped A_best=0.95/CVw_best=0.7 as universal SSA default
- Harvest has ZERO biological posterior columns (no beta/sigma/rho/chi/gamma/tau/mu_j/psi). Geometry
  tuned ONLY on fit metrics (r2,bias). Effect on biological posteriors never measured.
- Implied-CFR diagnostic (cfr_predicted_*/cfr_ratio_*) is v0.36+: 64/443 rows populated, ALL MOZ.
  v1's A=0.95 geometry had 0 CFR-instrumented runs (all 230 A=0.95/CVw=0.7 runs predate v0.36).
- Within 64 CFR rows (A=0.7-0.9 only) higher A_best -> HIGHER CFR over-prediction (1.5->3.3; thin).
- "deaths-r2 +0.29 with A_best" rests on r2_deaths_ensemble = median-method for 287/296 rows; the
  median metric MASKS the ~2x implied-CFR inflation (see [[central_method_v038]]). Cases-r2 +0.88
  did NOT replicate under controls (my pooled fit A=+0.046 p=.48; sign flips).
- profile="broad" (CVw=0.5) steers to A=0.95/CVw=0.5 cell, cases bias_ratio=6.26 — epi hazard.

## Round 2 (2026-06-17): epi block LIFTED for the rewritten (thinned) v2
v2 NO LONGER sets A_best/CVw_best/min_best_subset/max_best_subset/ESS_param_prop -> they fall
through to mosaic_control_defaults() = the 05-model-calibration.Rmd spec (A=0.70, CVw=1.0, ESS_best=
100, n_best~113). Verified: control defaults at R/run_MOSAIC.R:3400-3410 match spec line 187 EXACTLY.
The contested MOZ-tuned geometry is GONE; the `broad` profile is removed. **This resolves my entire
round-1 objection** (the objection was specifically against imposing the uncertified A=0.95 geometry).

v2 only sets: targets$ESS_param (always; reachable 0.18*n^0.72 fixed, or =T adaptive) and, IF the
user passes subset_size, targets$ESS_best=round(ss/1.13). NEITHER carries a biological/CFR claim:
- ESS_param is a convergence diagnostic (when to stop sampling); it does not pick WHICH draws weight
  the posterior, so it has no direct biological-posterior or implied-CFR consequence. Epi-neutral.
- ESS_best is the GENUINE subset-size lever (verified: grid_search_best_subset.R:174 stops at first n
  with ESS>=ESS_best AND A>=target_A AND CVw<=target_CVw; per-n temp rescale to effective_range=4.0
  makes A/CVw inert at sensible values so ESS_best binds; n_best~1.13*ESS_best). It changes ensemble
  WIDTH, not central geometry.

## ONE remaining epi CONCERN (minor, doc-level — does NOT block)
Ensemble size DOES touch the implied-CFR diagnostic via member count. calc_cfr_period_implied.R:90
floors the CFR-posterior CI at MIN_MEMBERS_FOR_CI=20; members = n_best * n_iter_ensemble (default 10).
- Default n_best~113 -> 1130 members: CFR CI well-resolved. Safe.
- subset_size FLOOR=30 -> ESS_best=30 -> n_best~34 -> 340 members (CI computed, >20) but only ~34
  DISTINCT posterior param sets. The across-member CFR CI then conflates 10x stochastic replication
  of a thin set of distinct draws -> a narrow CI that overstates biological-posterior precision on
  implied-CFR (and on any marginal: mu_j0, beta split, rho/chi). The 20-floor guards member COUNT,
  not distinct-draw count. Recommend the roxygen warn that small subset_size (<~80, i.e. n_best<~90)
  thins the biological posterior and can make implied-CFR CIs falsely tight; keep default >=100.
- Deaths/mean-method interaction unchanged from v0.38 by-design; v2 doesn't touch central_method.

## Deferred re-harvest (v2 explicitly defers; I CONCUR it's a genuine follow-up, not a ship-blocker)
Because v2 imposes no geometry, it is epi-SAFE to ship without the re-harvest. Re-harvest still
needed before any FUTURE geometry/subset DEFAULT is certified: biological posteriors + cfr_ratio +
mean-method metrics + >=1 endemic & >=1 epidemic country (current harvest is MOZ in-sample median).

How to apply: epi sign-off = SHIP v2 (block lifted). Conditions: (1) add the small-subset_size /
thin-posterior + implied-CFR-CI caveat to roxygen; (2) keep re-harvest as tracked follow-up before
re-introducing any geometry/subset default. See identity in [[cfr-mu-j0-identity]].
