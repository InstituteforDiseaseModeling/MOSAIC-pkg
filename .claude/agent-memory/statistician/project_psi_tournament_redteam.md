---
name: psi-tournament-redteam
description: Red-team of the 6-variant suitability-psi calibration tournament (v0447_smoke) that selected G as production default — statistically over-claimed
metadata:
  type: project
---

The v0.44.7 suitability-psi "tournament" (old/C/D/E/F/G/H, 15 countries, 1000 sims x 3 iters,
local PSOCK, deaths weight=0.05) selected G as production default on median r2_cases_ensemble.

**Why it matters:** the conclusion "G beats E" is within noise and NOT statistically supported.

**How to apply / the evidence (recomputed from output/v0447_smoke*/<ISO>/3_results/summary.json):**
- G vs E paired on r2_cases (mean method): mean diff +0.0017, t~0.16, sign-test p~0.61, G wins 9/15.
  Indistinguishable.
- The ranking FLIPS under estimator choice: under the median-method R2 (also reported in the same
  summary.json) E beats G head-to-head (G 7 / E 8) and the aggregate order changes (F moves to 2nd).
  Within-run |mean - median| R2 gap is 0.03-0.045 median (up to 0.117) — ~10x the claimed 0.004 G-E gap.
- ALL 7 variants: converged=FALSE in 15/15, ess_pct_above_target=0, median ess_min=7.8 (target 1000).
  Cannot reliably rank psi at this budget.
- ess_min is psi-INVARIANT (identical E==G in 12/15) — ESS driven by shared pinning/sampling, not psi.
- Headline metric is method="corr" (squared Pearson) = scale-INVARIANT shape agreement. It cannot see
  the 40-50% case / 230-270% deaths magnitude over-prediction the per-capita variants introduce
  (bias_cases 1.25 old -> 1.40-1.50 D/E/F/G; bias_deaths 3.09 -> 3.3-3.7). R2-up-while-bias-up is
  not a clean improvement.
- Attribution is confounded: G changes ~4 things vs E at once (D-target + AI already in E, plus
  rw_subsample=5 tiling + fit_date_start=2010 + n_seeds 5->10). Cannot isolate which helped.
- Seed-count differs across arms (old=3, D/E/F=5, G=10) — unequal MC averaging confounds the compare.

**Defensible claim:** the per-capita family (D/E/F/G) clearly beats old/H on shape (median ~0.69-0.72
vs 0.59/0.64). Choosing G *within* that family over E/F is a coin-flip dressed as a result.
