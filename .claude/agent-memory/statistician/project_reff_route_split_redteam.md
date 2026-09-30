---
name: reff-route-split-redteam
description: v0.92.0 R_eff=R_hum+R_env red-team (2026-09-28) — per-cohort env normalization depends on FUTURE delta (look-ahead, not instantaneous); direct path uses base config; IC-mask rescale unstable; delta>1 aborts
metadata:
  type: project
---

Red-team of commit d16d66f36 (branch feature/reff-route-split), R/calc_Reff.R.

**Invariant to remember:** under a time-varying reservoir decay delta_t, normalizing each infection
cohort's env profile by its OWN lifetime C(u) (row normalization) makes R_env[t] depend on delta over
[t, t+~3/delta] — anticipates seasonal fast->slow transitions. COD medoid t=1680: code 5.46 vs
Fraser past-only (column-normalized, b(t)*sum_tau A(t,tau)) 1.89 vs frozen-at-t (b(t)/delta_t) 2.19;
peaks inflated 2-3x vs Fraser; truncating the series changes R_env 80% at cutoff, 13% 240d earlier.
Cori's "conditions as at t" = frozen normalization; Fraser 2007 integral = column normalization.
Row==column==frozen only when delta constant. The sum R_hum+R_env itself IS exact (= mixture-kernel
Cori with weights R_route/R).

**Why:** production medoids are ~99.4-99.9% env-route, so R_eff ~= R_env and this hits the headline.

**Other gotchas found:** add_reproductive_numbers direct path loads 1_inputs/config.json (BASE prior
means) not config_medoid.json -> env kernel/delta wrong (ETH peak 4.83 vs 10.19). ic_rescale heuristic:
W_hat/W_median ratio drifts 4-13x over time, ic_start jumps 254->804 between tol 0.9 and 0.95.
decay_days_short prior a=0.01 -> delta>1 (0.5% ETH samples); engine clamps, calc_Reff stop()s.

**How to apply:** when reviewing any time-varying-kernel renewal estimator, check row vs column
normalization and test truncation invariance. Scratch: claude/reff_review/redteam/stat/ (worktree).
