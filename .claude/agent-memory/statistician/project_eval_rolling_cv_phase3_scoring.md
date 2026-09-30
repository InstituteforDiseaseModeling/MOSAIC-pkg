---
name: eval-rolling-cv-phase3-scoring
description: evaluate_rolling_cv() Phase-3 scoring upgrade — renamed skill columns, ESS gate, true WIS-skill, NA-gated seasonal baseline, per-metric embargo
metadata:
  type: project
---

Phase-3 scoring upgrades to `evaluate_rolling_cv()` (R/evaluate_rolling_cv.R), per
claude/plan_forecast_cv/PLAN.md §3. All additive/back-compat (defaults preserve prior behavior).

**Column-name break (the one non-back-compat change, justified by Task 3):**
- `$cells`: `skill_<b>` is GONE → split into `mae_skill_<b>` (old MAE-ratio, renamed) +
  `wis_skill_<b>` (new). Also new: `ess`, `ess_ok`.
- `$summary`: `n_cells` GONE → `n_cells_used` / `n_cells_total`. New: `ess_gated`,
  `cov50_calib_err`, `cov95_calib_err`, `ci_suppressed`, and per-skill `_mean/_lo/_hi` keyed on
  the renamed mae_skill_/wis_skill_ cols.

**Why each matters:**
- ESS gate (`ess_min`, default 0=off): reads per-cutoff `ess` col; `ess_ok = !is.na & >=ess_min`.
  Failing cells STAY in `$cells` (flagged) but are EXCLUDED from `$summary`. No `ess` col →
  ess_ok=TRUE (back-compat), ess_gated=FALSE.
- Seasonal baseline NA-gate: requires >=2yr IS span (730d) else NA — the silent grand-mean
  fallback was REMOVED. Seasonal = primary baseline. Added `persistence_last` (single last value)
  alongside trailing-4 `persistence`.
- True WIS-skill: baselines now get 50/95 PIs from empirical IS-residual quantiles (type=7);
  `wis_skill = 1 - WIS_model/WIS_baseline` via the shared `.rcv_wis` (Bracher 0.5*MAE, /2.5).
  Needs >=2 finite residuals to form an interval, else wis_skill=NA.
- Small-n CI suppression (`min_cells_ci`, default 5): <min finite cells → lo=hi=NA, point kept,
  ci_suppressed=TRUE. (A 1-cell group is NA-CI regardless but ci_suppressed only flips on the
  threshold.)
- Per-metric embargo (`embargo_weeks` scalar OR named c(cases=,deaths=)): OOS start =
  cutoff + emb[metric]*7; scoring uses `date >= oos_start & date > cutoff` (segment label is NOT
  the authority). Scalar 0 = prior behavior (date > cutoff == segment=="OOS").

**How to apply:** any downstream consumer reading `$summary$n_cells` or `$cells$skill_*` will
break — grep before wiring. plot_rolling_cv only reads R2_corr + wis (untouched), so it's safe.
Scope was eval fn + its test ONLY (no DESCRIPTION bump, no document()).
