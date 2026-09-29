---
name: nd-dlinear-evidence-audit
description: Audit of the ND/DLinear-trunk arm in psi_evolve (2026-09-24) — the 13% win is estimand-specific and reverses at weeks 9-13; NOT ready to ship as a default
metadata:
  type: project
---

`PLAN_ND_PRODUCTION.md` proposes shipping `architecture = "dlinear_v1"` in `est_suitability()`.
Audit verdict: **ship as a documented experimental option only; do not default**.

**Why:** the headline "-13.3% vs LSTM" is raw-psi burden-weighted MAE over the FULL 13-week test
window (`accuracy_table.R` / `per_country_weakness.R`, 6 selection blocks, 16 countries). The
programme's confirmation endpoint (`PREREGISTRATION_CONFIRM.md`) is the persistence-BLEND MAE at
**weeks 9-13**, which is what `null_gate_results.tsv$real_wk913` reports
(`diag_blend_needs_psi.R:117,134` — the column scores `blend`, not `psi`, and psi is first
re-levelled onto persistence at line 84). **The sign flips:** ND 0.17619 is the WORST of all 19
arms; every DLinear arm loses to every LSTM arm except T2. ND also loses to its own
per-country-block constant (0.17505). ND's raw-MAE edge is LEVEL, and the blend takes its level
from persistence, so the edge cannot transfer — the same mechanism as `psi_star_b` absorbing it
downstream (BACKLOG DS-02).

Other load-bearing facts:
- **ND has no `S`** — REGISTRY.tsv rows 151/152/154/156 have empty `S`/`boot_lb`/`n_beat`, so no
  A1-A6 adoption verdict from `compare_arms.R` exists (PROTOCOL §3: a verdict from anywhere else
  is not a verdict). A6 (must beat week-of-year climatology) is unmet: 0.1772 vs 0.1782.
- **ND was fitted with `country_static="off"`, `country_balance=FALSE`** (`run_arm.R:155-156`),
  but production defaults are `"auto"`/`TRUE` (`run_rolling_cv_suitability.R:169-172`). Shipping
  `dlinear_v1` yields an untested dlinear+D9b+N8 combination.
- Per-country sign: ND better in 10/16 countries, exact two-sided p = 0.4545 (74% of burden
  weight). Range -45.8% (NGA) to +429.9% (CMR). The plan quotes NDe's -49%/+598% while
  recommending ND be pinned.
- Cost: ~19-24 min vs 42 min per arm at 10 seeds (REGISTRY `dugong_h` 0.4 vs 0.7), i.e. 2.2x, not
  the 13x that the per-fit-unit smoke figure suggests.

See [[psi-replicate-floor-is-family-specific]].
