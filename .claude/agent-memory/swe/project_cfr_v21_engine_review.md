---
name: cfr-v21-engine-review
description: CFR v2.1 (fate-at-onset mu_jt) engine review 2026-09-28 — engine math verified; traps = unrebuilt config_default silently takes legacy path, two mu_jt resolvers, warn-once test-order, freeze-at-T not leakage-strict
metadata:
  type: project
---

Reviewed the uncommitted CFR v2.1 worktree (branch feature/cfr-v21-mujt, base 9d215ba47) on 2026-09-28.

**Engine verified exact** (scratch harness `claude/cfr_v21_review/swe/*.R` in that worktree): mu_jt=0 vs base mu_j_baseline=0 gives 28/28 channels identical() (rbinom p=0 draws nothing); rho_deaths=1 gives reported_deaths[,c]==disease_deaths[,c-lc]; ΔN_c = births_c − ndd_c − dd_{c+1} (onset deaths land ONE column later than hazard mode); IC-only cohort (iota=0) → 0 deaths; realized CFR/mu within ±2.5% at chi=1.

**Traps worth remembering:**
- A legacy fallback that triggers on `!is.null(config$mu_j_baseline)` makes an UNREBUILT `data/config_default.rda` run green on the legacy constant-CFR path, so the "new model" is never exercised by the default. Always check the .rda actually carries the new field (`load()` it), not just that data-raw was edited.
- `.sim_mu_jt()` (engine) and `.mosaic_config_mu_jt()` (likelihood/sandbox) are two resolvers of the same rule and already diverge (legacy-without-CFR_target, [nT x nL] orientation). Lesson #11 shape.
- A session-scoped `.mosaic_warn_once()` makes `expect_warning()` tests order-dependent.
- Freezing a logit-interpolated annual series at cutoff T is not leakage-free: the value at T already interpolates toward mid-year(T)/(T+1) anchors, and the GAM fit uses all years.

See [[project_forecast_cv_phase1_psi_cache]] for the rolling-CV cutoff plumbing.
