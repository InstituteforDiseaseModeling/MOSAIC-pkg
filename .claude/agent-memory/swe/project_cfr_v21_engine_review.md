---
name: cfr-v21-engine-review
description: CFR v2.1 (fate-at-onset mu_jt) engine + red-team review 2026-09-28 — engine exact/bit-identical replay; traps = post-hoc redraw stop() drops whole ensemble members, redraw RNG not kind-pinned, 40-loc integration +9% worker/+17% ensemble, mismatched deaths_integration opaque failure
metadata:
  type: project
---

Reviewed CFR v2.1 on branch feature/cfr-v21-mujt (base 9d215ba47, HEAD 6e862d9ab v0.96.1) on 2026-09-28.

**Engine verified exact:** replay digests bit-identical to base on all 4 fixtures; mu_jt=0 vs base mu_j_baseline=0
gives 28/28 channels identical() (rbinom p=0 draws nothing); onset deaths land ONE column later than hazard
mode (ΔN_c = births_c − ndd_c − dd_{c+1}); integration exposure alignment s = delta_reporting_cases + 1 matches
engine draws (sum ratio 0.999-1.003, lc 0/3/7). HEAD engine is ~2% FASTER than base (hazard block skipped pays
for the fatal_onsets draw, 4.4 ms/run).

**Red-team findings at HEAD (may since be fixed — check before acting):**
- `.mosaic_posthoc_deaths()` stop()s when the Laplace-mode CFR implies p>=1 in any location; the ensemble task's
  tryCatch then drops the WHOLE member (cases too), and n_successful is not in run.log/summary.json. 11/30
  prior-draw members at 40-loc config_default; 0/565 in a calibrated NGA ensemble. Root: offset unconstrained on
  logit(mu) while feasibility needs mu < rho_d*chi_epi/rho.
- Redraw uses set.seed() in the caller's RNGkind (engine pins Mersenne-Twister via .sim_rng_begin) → a
  L'Ecuyer master changes deaths/cfr_posterior, breaks seq==par reproducibility.
- 40-loc cost: worker +9.1%/sim, post-hoc +17%/member; rowsum/tapply/unique dominate `.d7_fit_location`;
  cumsum-run-ends rewrite = 0.58x (verified to 2e-10). 1-loc ≤1.2%.
- Weekly NB (k~0.8) Laplace mode overshoots observed deaths level on poor paths (NGA prior draws up to 19x).

**Traps worth remembering:**
- A legacy fallback keyed on `!is.null(config$mu_j_baseline)` lets an UNREBUILT .rda run green on the legacy path;
  `load()` the .rda to confirm the new field ships.
- Production medoids under MOSAIC-results/models/* were made by MOSAIC 0.55.x (Python engine): under the R engine
  the MOZ medoid gives 20 onsets vs 72,809 observed — useless as "calibrated path" proxies. Run a short
  run_MOSAIC smoke instead (1,500 sims NGA ≈ 3 min on 4 PSOCK workers with an installed build).
- Freezing a logit-interpolated annual series at T is not leakage-free unless the GAM is refit on years <= T-1
  (v0.96.1 does this: .rcv_cfr_asof).

See [[reference_paired_timing_and_worktree_snapshot]] for the timing/snapshot method and
[[project_forecast_cv_phase1_psi_cache]] for rolling-CV plumbing.
