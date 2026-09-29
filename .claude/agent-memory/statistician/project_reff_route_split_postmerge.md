---
name: reff-route-split-postmerge
description: Post-merge audit of route-decomposed Cori R_eff (v0.92.1, PR #126) - estimand math verified exact; peak_Rt is small-denominator noise; engine tests blind to the FOI lag
metadata:
  type: project
---

Audited merge 96096a659 (v0.92.1) on 2026-09-28. Math is CORRECT: Lambda_hum = lag1(I_hat)/D_h and
Lambda_env = lag1(W_hat)*delta_t/S_w match an independent brute-force engine-order recursion to 1e-13
(random params, time-varying delta incl. >1, nonzero E/Is/Ia init, Tn=1..200); sum_k Ps_k = sigma/p1
holds because arrivals are not recovered on arrival (engine: rn$Isym = is_next + new_sym after the recovery draw).

**Env-test ratio ~0.94 is NOT estimator bias.** Decomposes EXACTLY (1e-15) into
W_realized/W_hat x (1-e^-psi)/psi (~0.994) x S_pre/S_post (~1.01) x binomial noise. Across 6 seeds the median
ratio spans 0.92-1.01 (mean 0.967); 2.6 pp of it is the TEST's truth omitting disease mortality
(mu=0.01 with gamma_1=0.2); the estimator itself is mortality-insensitive (est(gamma1)/est(gamma1+mu)=1.001)
because W_hat and S_w inflate together.

**Why:** traps to remember.
- peak_Rt (daily time-max, floor=1) lands where Lambda_hum ~1-3 and incidence 3-15 in 15/18 location-runs; floor 1->10
  halves it (~5.2 -> ~3.3). It is an extreme of Poisson noise unless window-aggregated (Cori tau=7: sum inc / sum Lambda).
- Mutation test: removing or doubling the lag in Lambda_hum passes every engine-truth test (median of I[t-1]/I[t] ~1 over
  a rise+fall window); only the hand-derived kernel-mean test catches it. Removing E0 init passes ALL tests.
- .mosaic_reff_member_quantiles: members dropped for a missing day keep weight NA, so the ">=50% weight" rule is
  relative to complete members only, not total posterior weight.

**How to apply:** if asked to trust peak_Rt/explosivity or tighten these tests, start here. Related: [[reff-median-aggregation-flattening]].
