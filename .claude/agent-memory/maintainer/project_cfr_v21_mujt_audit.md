---
name: cfr-v21-mujt-audit
description: CFR v2.1 (v0.96.0/.1) fate-at-onset + time-varying mu_jt + integrated deaths LL — completeness audit + red-team; which wiring has NO test, the missed estimated_parameters sibling set, out-of-package breakers
metadata:
  type: project
---
Two passes, 2026-09-28 (worktree cfr-v21, base 9d215ba47 v0.95.0): an uncommitted-state audit,
then a red-team of HEAD 6e862d9ab (v0.96.1). Engine+likelihood core is sound (Laplace marginal
vs 2-D quadrature < 0.02; onset/report alignment verified; realized reported CFR 0.1513 vs 0.15 at
forced epidemic PPV, 0.1002 vs 0.100 at forced endemic).

**Why:** reusable traps for any future change to the mortality/observation model, or any
"integrate a parameter out instead of sampling it" change.

**How to apply:**
- First-pass items all fixed by v0.96.1: one legacy resolver `.mosaic_mu_jt_matrix` (keys on
  mu_j_baseline/mu_j_epidemic_factor/CFR_target/mu_j); config_medoid.json shifted to the posterior
  CFR; ensemble mock formals; builders coherent; `.SIM_REPLAY_ONLY_SITES` test-consumed.
- An integrated-out param is absent from samples.parquet/config_medoid. Re-simulators outside
  run_MOSAIC get PRIOR deaths unless they pass `deaths_integration`: still true for
  MOSAIC-OCV run_scenarios.R ensemble mode.
- Mutation results at 6e862d9ab: dropping the worker's integrated-deaths call survives the FULL
  suite + opt-in integration test; dropping the medoid shift survives the default suite (only the
  MOSAIC_RUN_INTEGRATION=1 test catches it); chi_epidemic->chi_endemic in `.sim_p_fatal` survives
  (all identity fixtures set chi_endemic == chi_epidemic). Alignment, quadrature, eps-bypass and
  rolling-CV leakage tests all go red when mutated.
- Missed sibling set: estimated_parameters inventory/.rda/doc (7 retired rows, stale counts);
  validated patch lives in claude/cfr_v21_review/redteam/maint/.
- Shipped config v5.0/priors v16.0 came from a gitignored delta script (claude/…/apply_v21_deltas.R),
  not the tracked builders = provenance gap.
- MOZ country repo configs (mu_j_baseline, no CFR_target) are REFUSED by v0.96 by design.
- Worktree was live-edited during the red-team: snapshot before running anything.
Related: [[cfr-r6-hygiene]], [[reviewer-checklist]], [[rcmdcheck-baseline-v048]].
