---
name: inference-scaling-measured-v0903
description: Measured @ v0.90.3 — MOSAIC's posterior does not improve with n. |B| and ESS_B are closed-form functions of control$targets$ESS_best; exact IS ESS is pinned at 1.00 from n=500 to n=100,000; posterior CI width is 96-98% of prior at every n; effective saturation n~7k-10k
metadata:
  type: project
---

Measured 2026-09-17 in the inference-methods review (agent SCALE), by nested row-subsampling
(n = 500…100,000, 30 replicates per n, 362 cells) of two production runs through the package's
OWN selection/weighting path (`get_default_subset_tiers` -> `grid_search_best_subset` ->
saturated `pmin(delta,4)`/eta=0.5 -> `calc_model_ess` / `calc_is_diagnostics` /
`weighted_quantiles`). Harness reproduced both run.logs exactly at full n (|B|=115,
ESS_B=107.475, A=0.98574, CVw=0.56197, khat=73.8786, retained 95,344).
Runs: `dugong:~/prod100k_v087` (40 loc x 100k) and
`/Users/johngiles/MOSAIC/output/eth25k_v0903` (ETH x 25k). Report:
`MOSAIC-pkg/claude/review_inference/findings/SCALE.md`; scripts/figures in
`.../review_inference/scratch/SCALE/`.

**Why:** "run more simulations" is the reflexive answer to a bad fit, and every number the
pipeline prints (ESS_B, A, CVw, ESS_param) supports it. None of them measure what they appear to.
Re-deriving this from the formulas gives the wrong answer; it has to be measured.

**How to apply:** before recommending more draws, check which of these is binding. Re-measure if
`ESS_best`, `best_subset_weighting`, `nb_k_min` or the sampler mode (FIXED vs adaptive-batch)
has moved — both reference runs were single-batch FIXED, i.e. i.i.d. from one prior.

1. **|B| ~ 1.13-1.15 x `control$targets$ESS_best`, independent of n and of the data.**
   Measured at ESS_best = 30/50/100/200/300/500 -> |B| = 37/59/115/230/341/563 (ETH 25k) and
   36/58/115/226/338/557 (PROD 100k). Mechanism: `grid_search_best_subset.R:137-155` rescales
   eta by the subset's own dAIC range, so the weights are `exp(-2 d_i)`, `d_i in [0,1]`, and
   **max/min weight is exactly exp(2) = 7.389056 for every subset regardless of the data**
   (verified on top-50/115/500 of ETH: dAIC ranges 2,818 / 4,215 / 7,904, same ratio to 6 d.p.).
   ESS/A/CVw then depend only on the normalized extreme-value shape, so the search stops at
   ESS/|B| ~ 0.87 every time.

2. **ESS_B is a closed form in |B|.** Exactly ONE member of B has dAIC <= 4; the other |B|-1 sit
   on the exp(-2) floor. For "1 unsaturated + (|B|-1) saturated": |B|=115 -> ESS_perp = 107.4751,
   ESS_kish = 87.3990 — which is what BOTH production runs report, to 7 s.f., at 25k/1 location
   and 100k/40 locations. Realized ratios are 0.935 (perp) / 0.760 (kish), NOT the 0.62/0.42
   worst-case bounds (those are attained when ~10% of members escape saturation). A and CVw are
   algebraically redundant: `CVw = sqrt(|B|/ESS_kish - 1)`, `A = log(ESS_perp)/log(|B|)`.

3. **Exact IS ESS = 1.00 in all 362 cells**, Kish and perplexity, n = 500 -> 100,000, both runs;
   log-log slope exactly 0 on PROD. Non-underflowing importance ratios: 1-2 at every n on PROD
   (1 of 100,000 at full n); 2 -> 14 of 25,000 on ETH. Cannot be fixed with compute: `log w = LL +
   const`, so sd(log w) = sd(LL) = 6.9e5 (ETH) / 4.2e5 (PROD); `n = 100 exp(sigma^2)` gives
   10^(2e11) / 10^(4e10) draws. SAMP's independent extreme-value route agrees (d_eff 4.93,
   KL 22.1 nats, n ~ 7.9e10).

4. **No variance reduction, ever.** Posterior 95% CI / prior 95% CI = 0.962 -> 0.976 (ETH, 50x n,
   log-log slope **+0.0042**, i.e. WIDER) and 0.972 -> 0.963 (PROD, 200x n, slope -0.0021).
   Some parameters come back wider than prior (`zeta_1` 2.02x on PROD).

5. **Random-subset null is the test that settles it.** 115 draws chosen at random, ignoring the
   likelihood, sit at W1 = 0.103 prior-SD from the prior; two such subsets differ by 0.146.
   The PROD posterior is inside that 95% null band at EVERY n from 500 to 50,000.
   W1(post_5k, post_100k) = 0.158 vs null 0.146 [0.119, 0.177] -> the 5k/100k difference is
   ~92% resampling noise. **Always compute this null before claiming a posterior moved.**

6. **Effective saturation n ~ 7,000 (1 loc) / ~10,000 (40 loc)** — where W1(post_n, post_full)
   over the *identified* parameters drops below the two-random-subsets floor. Both production
   runs are 2.5x-10x past it.

7. **Dimension, not weights, is the binding constraint.** A properly tempered IS
   (`w ∝ exp(eta*LL)`, eta solved for exact Kish ESS = 100) gives eta ~ n^0.54 (ETH) / n^0.33
   (PROD) and DOES narrow the ETH CI (slope -0.0108 vs +0.0042) — but shows **no significant
   improvement with n at all on the 40-location run**.
   **CORRECTION (2026-09-17, agent WEIGHT): do NOT act on this by setting
   `best_subset_weighting = "tempered"`.** The scheme measured above is ESS-TARGETED tempering
   (eta solved for a target ESS). The `"tempered"` control switch is a DIFFERENT estimator —
   `.mosaic_calc_adaptive_gibbs_weights()`, `eta = -log(1e-15)/max(Delta)` — and on the ETH
   best subset it gives **ESS_perp = 1.25 and puts 96.08% of the mass on one draw** (vs 107.48
   / 6.09% for `"saturated"`), because within a 115-draw subset max(Delta) = 4,215 rather than
   2.88e6, making eta 680x larger. It is 86x SHARPER, not softer, despite its doc string.
   ESS-targeting must be implemented; it is not reachable from the config.
   The per-location evidence: ETH's 10 location parameters are identified 7/10 by a 25k
   single-country run and **1/10** by the 100k 40-country run (same priors, verified within
   0.024 prior-SD). Joint rank-selection dilutes per-location selection pressure ~40-fold.

8. **`ess_marginal` (the `ESS_param` gate) is still likelihood-blind at v0.90.3.** Shuffling the
   likelihood column across rows moves the median by **0.0-4.1%** (500: 91.5 vs 91.5; 25,000:
   1743.1 vs 1741.3). It is 0.07-0.18 x n and crosses the default target of 100 at n ~ 600-700.
   Confirms and updates the v0.85.0 measurement in [[weighting-stack-measured-v085]].

9. **`05-model-calibration.Rmd:249` is wrong**: "the truncated dAIC weights are proportional to
   the posterior density" does not hold — within B the true density ratio spans exp(-52,263)
   (PROD) / exp(-2,108) (ETH) while the assigned weight ratio is capped at 7.39. The spec's
   `:210-233` saturation description IS now accurate (it was updated); the proportionality claim
   at `:249` and the Importance-Sampling framing at `:193` were not.

Best-LL scaling (the one thing that does improve): `ll_1 = L* - c n^-g`, g = 0.414 (ETH) /
0.318 (PROD); ceiling L* = -17,089 / -377,058; gap at full n = dAIC 547 / 16,846; n to come
within dAIC 4 of L* = 3.9e9 / 5.2e16. The ensemble's internal LL spread falls only as n^-0.44 /
n^-0.29 (11,870 -> 2,108 and 247,412 -> 52,263).

Related: [[weighting-stack-measured-v085]] (nb_k_min binds and over-sharpens, which inflates
sd(log w) and therefore item 3 — magnitude not measured).
