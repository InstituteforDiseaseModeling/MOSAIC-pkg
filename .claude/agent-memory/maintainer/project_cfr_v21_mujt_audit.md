---
name: cfr-v21-mujt-audit
description: CFR v2.1 fate-at-onset + time-varying mu_jt + D7 integrated deaths LL (v0.96.0 WIP) completeness audit — sibling sets, legacy-mu_jt guard sites, config_medoid uncalibrated-deaths trap, mock-signature drift
metadata:
  type: project
---
Audited 2026-09-28 (worktree cfr-v21, base 9d215ba47, uncommitted). Engine+likelihood core was sound
(Laplace marginal matched a 2-D brute-force integral to 1e-3; the lc+1 death/onset column alignment was verified empirically);
the gaps were all in the surrounding plumbing.

**Why:** the reusable traps for any future change to the mortality or observation model.

**How to apply:**
- Legacy-config guard sites that must agree: `.sim_mu_jt` (sim_params.R: converts CFR_target), `make_simulation_config` (REFUSES any mu_j_*),
  `.mosaic_config_mu_jt` (needs mu_j_baseline AND CFR_target, else silently falls to the dead legacy mu_jt), run_fit_sandbox (tryCatch→silent).
  All of them key on `mu_j_baseline` only, so a pre-IFR config (`mu_j` + dead mu_jt) passes straight through.
- A parameter that is integrated out (not sampled) is NOT in config_medoid.json / samples.parquet. Anything that re-runs a saved config
  (`.rcv_simulate_config` in run_rolling_cv.R, plot_mean_medoid, dugong scripts) gets PRIOR deaths, not calibrated ones. Only
  calc_model_ensemble(deaths_integration=) applies the post-hoc redraw.
- Adding an arg to `.mosaic_ensemble_sim_task` breaks tests/testthat/helper-ensemble-mock.R `fake_task` (fixed formals), which takes down
  calc_model_ensemble, tier2_parity, trajectories and seed_alignment together (about 25 errors from one cause).
- data-raw/make_config_default.R was left half-edited (it still references the removed mu_j_baseline/CFR_target objects), and the .rda files were not rebuilt.
  Always check that the builder actually runs; a diff that reads clean is not enough.
- Dangling roxygen refs: sim_rng.R cites test-sim-mortality-onset.R (does not exist); `.SIM_REPLAY_ONLY_SITES` is defined but has no consumer.
Related: [[cfr-r6-hygiene]], [[reviewer_checklist]].
