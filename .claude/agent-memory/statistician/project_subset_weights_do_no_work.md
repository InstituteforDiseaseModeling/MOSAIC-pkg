---
name: subset-weights-do-no-work
description: Within the best subset the likelihood weights carry no predictive information (uniform == saturated), and predictive skill DECREASES monotonically as the likelihood is allowed to concentrate; true IS weights give R2 0.19 vs 0.54. Ablation @ v0.90.3 on the 100k run.
metadata:
  type: project
---

Holding the member set fixed at the top 115 and re-deriving the per-cell weighted median under
seven weight vectors (100k run, `dugong:~/prod100k_v087/`, v0.90.3):

| scheme | ESS_perp | MAE score | R2_cases | R2_deaths |
|---|---|---|---|---|
| saturated `pmin(d,4)` (SHIPPED) | 107.5 | -2.10896 | 0.5433 | 0.3448 |
| **uniform** | 115.0 | -2.12818 | **0.5516** | 0.3528 |
| range-normalised `eta=2/range` | 100.8 | **-2.07005** | 0.5446 | **0.3639** |
| `pmin(d,25)` | 1.006 | -4.02886 | 0.1882 | 0.1249 |
| `pmin(d,100)` | 1.000 | -4.11543 | 0.1876 | 0.1198 |
| **untruncated (TRUE IS weights)** | 1.000 | -4.17491 | **0.1876** | **0.1193** |
| `w ~ 1/rank` | 43.7 | -2.39484 | 0.4740 | 0.2720 |

**Two invariants:**

1. The three near-uniform schemes are **statistically indistinguishable** (within 0.008 R2, no
   consistent winner). Replacing the likelihood weights with *exactly uniform* costs nothing.
2. **Predictive skill decreases monotonically in how much the likelihood concentrates the
   weights.** Saturating at 25 instead of 4 already collapses ESS to 1.006 and R2 to 0.19.

**Why it matters:** the pipeline's predictive performance comes from **ensemble averaging**, not
from Bayesian weighting. The likelihood maximiser is a far worse predictor than the average of
the top 115 — a direct, quantified statement of likelihood misspecification, and the reason the
`Delta*=4` saturation exists at all. The `"tempered"` alternative
(`control$targets$best_subset_weighting`) is NOT a middle ground: measured ESS 1.255 of 115 on
ETH, i.e. also a point mass. There is no setting that yields an informative non-degenerate
weighting, because the defect is in the likelihood, not the weighting.

**Scaling (the answer to "does more n help?"):** ESS_B is PINNED at ~107.5 from n=500 to
n=25,000 (measured 106.5 -> 107.5 over a 50x increase). A fixed-temperature fractional posterior
`w ~ L^zeta` over ALL retained draws instead gives ESS 3.6 -> 100.0 over the same range, i.e.
**ESS proportional to n**. Current scheme's implied `zeta = 4/Delta_(K) = 9.5e-4` ~ **2.65
effective observations** for ETH.

**How to apply:** do not tune the within-subset weighting hoping for fit gains — it is a
0.008-R2 lever. If someone proposes "sharpen the weights to use the likelihood better", this
table says it costs 65% of R2. The real levers are the member SET and the likelihood itself.
Never quote `ESS_B` as coverage evidence — see [[project_best_subset_selection_is_data_free]].

Theory anchor: Biau, Cerou & Guyader 2015 (arXiv:1207.6461) — ABC IS a k-NN estimator;
consistency needs `k_N/log log N -> inf` AND `k_N/N -> 0`. MOSAIC has k fixed, so it converges to
a point mass at the prior-constrained MLE, not to the posterior. Measured: 100x more draws moves
the median marginal posterior width ratio by -0.0021 (0.9642 -> 0.9621).

Full report: `/Users/johngiles/MOSAIC/MOSAIC-pkg/claude/review_inference/findings/SUBSET.md`.
