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

**Update 2026-09-29 (v0.93.0 review):** the v0.92.2 fix (branch fix/reff-postmerge-review, a0d7ff51a) is
UNMERGED, so main still has daily peak_Rt, parLapplyLB hang, and 15-digit `1_inputs` JSON (verified:
2/6 members of a 4-loc config diverge 0.4-1.6% total cases -> recompute_ci gate refuses multi-loc runs).
config_medoid.json is written at 15 digits even on the branch. recompute_ci uses ensemble_candidate
(tier weights) because optimize re-sorts p (breaks p*1000+s seeds); correct fix = resim candidate,
reweight by weight_best_opt. Kernel ignores mu: Isym survival is exactly exp(-(gamma_1+mu)); mean_hum
9.12/8.04/6.53 d at mu 0/0.017/0.058.

**Update 2026-09-29 (v0.99.9 deep review):** v0.92.2 fixes ARE merged (0d8d95581, v0.99.5): 17-digit
1_inputs JSON, 7-day windowed peak_Rt, robust gather. Onset-death (CFR v2.1) omission in the kernel
verified: +0.10/0.22/0.36% R_hum bias at r=0.02/0.05/0.10 (p_fatal 2.8%), +1.3% at p_fatal 10%.
STILL OPEN: recompute_ci resims ensemble_candidate -> with optimize_subset=TRUE its medoid/weights are
NOT run_MOSAIC's final (optimized) posterior; per-channel central_method loses names in control.json
-> silent median fallback; any member resim error aborts the whole CI.
