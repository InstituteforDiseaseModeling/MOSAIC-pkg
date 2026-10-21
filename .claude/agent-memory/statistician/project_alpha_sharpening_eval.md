---
name: alpha-sharpening-eval
description: 2026-10-19/20 alpha 10/20/30/50 + top-K + progressive-subset (G100-300) re-ensemble of FCV-PROD6 (52 cells) and 28 production runs; OOS differences are at the Monte Carlo seed-noise floor; harness reproduces production bit-for-bit
metadata:
  type: project
---

Re-ensembled existing draws (no recalibration) with calc_model_ensemble() under MOSAIC 1.3.0
(lib ~/R/lib_v130 on dugong, scripts claude/weights_investigation/alpha_eval/, outputs dugong
~/alpha_eval/out). Option E (stored saturated weights = top-108, exp(-0.5 min(dAIC,4))) reproduces
the original FCV-PROD6 ensemble rows bit-for-bit in 52/52 cells and the headline 0.346 [0.185, 0.392].

**Findings (CONFIRMED):** OOS cases WIS-skill per-cell paired vs E: A +0.009 [-0.002, 0.033],
D (alpha 50) -0.013 (worse 32/52; one AGO cell -11 skill from a ~4-effective-member false outbreak),
all others |d| <= 0.004. Seed-only replicate Er (E members reversed -> new position seeds) gives
+0.0025 [0.000, 0.005], 32/17 better/worse: pure noise looks "significant", so option differences
<= ~0.005 median skill are unresolvable with 52 cells. OOS cov95 ~0.85-0.87 (under nominal) while
in-sample daily cov95 ~0.99 / cov50 ~0.82 (over-cover): sharpening narrows the wrong side.

**Traps:**
- calc_model_ensemble seeds members by POSITION (param_idx*1000+stoch), so the same draw gets
  different sims in different member lists; paired option comparisons carry seed noise -> always
  run a reorder replicate (Er) as the noise floor.
- The forecast-year CFR pilot changes IN-SAMPLE deaths too (Laplace couples years via smooth
  year deviations): skipping it moved in-sample deaths_mean up to 18%. Cases unaffected.
- cluster_boot CI depends on the ORDER of unique cutoffs fed to sample(): FCV score.R order is
  latest-first; ascending order moved the E lower bound 0.185 -> 0.169.
- Pooled median of cell skills is not an option-comparison statistic (alpha 50 had the highest
  median 0.381 while losing 32/52 paired cells); use paired per-cell differences.
- pgrep/pkill -f over ssh matches the ssh command line itself and kills the session.
- A Kenya stage-2 job (170 PSOCK workers) was started on dugong mid-run; renice +19 own tasks.
