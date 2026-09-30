---
name: required-n-prior-as-proposal
description: BFRS proposal is the FROZEN prior (no adaptation, contra the spec); prior-as-proposal needs ~1e11 draws for ESS=100 on ETH even noise-free, and 25k->100k closes only 43% of the best-draw gap while |B| stays 115
metadata:
  type: reference
---

Measured 2026-09-17 on ETH x 25,000 (MOSAIC v0.90.3). Scripts:
`MOSAIC-pkg/claude/review_inference/scratch/SAMP/0[1-3]*, 22*`.

## The proposal (code fact)
`sample_parameters(priors = priors, seed = sim_id)` (`R/run_MOSAIC.R:284-291`). `priors` is bound
once as a formal of `run_MOSAIC()` and **never reassigned anywhere** in `run_MOSAIC.R` /
`run_MOSAIC_helpers.R`; it is `clusterExport`ed once at `:1221`, before the batch loop.
`.mosaic_decide_next_batch()` (`run_MOSAIC_helpers.R:1538-1620`) returns only `phase` and
`batch_size`. **"Adaptive" = adaptive SAMPLE SIZE, not adaptive proposal.**
`update_priors_from_posteriors()` is offline-only, no caller in the loop.

i.i.d.-ness verified empirically (consecutive integer seeds): max |lag-k| autocorrelation 0.017
over k=1..10 x 12 params (2-sigma = 0.0124 with 120 comparisons), max off-diagonal cross-corr
0.0126, no likelihood drift with sim_id (p=0.49).

**`05-model-calibration.Rmd:161` says otherwise** -- it promises a "fine-tuning phase ... from an
updated proposal distribution centred on the retained subset". Not implemented. Ironically the
implemented behaviour is SAFER: the documented version would pool draws from different proposals
under one weight formula = real uncorrected mixture bias. If it is ever built it needs a
defensive-mixture weight `w_i = L_i*pi(theta_i) / sum_b alpha_b q_b(theta_i)`.
The same paragraph's "milliseconds per draw" is also stale: measured **~1.05 s/iteration** on the
pure-R engine at ETH width.

## Required n
Sample-based divergence estimates are SATURATED and useless: `KL_hat = max logL - log Zhat =
10.1266 nats = log(25000)` EXACTLY; only 14/25,000 draws have non-underflowing ratios.
Estimate instead from the growth of the running max (power law, R^2=0.969):
- tail index `2/d = 0.4054 +/- 0.0295` => `d_eff ~ 4.9`; gap from the n=25,000 best draw to the
  extrapolated optimum = **303 nats**
- `KL_theta ~ 22-24 nats`, `D2_theta ~ 21-22` => **n for ESS=100 = 1.9e11 - 3.2e11**
  (Agapiou, Papaspiliopoulos, Sanz-Alonso & Stuart 2017, Statist. Sci. 32(3):405-431:
  ESS/n -> exp(-D2), n_req = ESS_target * exp(D2))
- independent cross-check, "n before the best draw reaches within 1 nat of the optimum":
  **3.3e10**. Bootstrap: n = 7.9e10 [7e8, 2e14].

**Add the score noise ([[likelihood-noise-floor]]) and the requirement becomes ~1e8160, i.e. no
finite n.** Quote BOTH decompositions.

## What 25k -> 100k actually buys
Gap to optimum **303 -> 173 nats (43% closed)**; `|B|` is set by `control$targets$ESS_best`, not
by n, so it is **115 at both**. Quadrupling n gives a slightly better best draw and gives the
posterior nothing.

## Dimension bookkeeping
The diagnostics show 58 varying columns but only **43 are free draws**; 15 are deterministic
(zeta_2, decay_days_long, beta_j0_hum/env, mu_j_baseline, six *_j_initial counts, four cfr_*),
all verified to machine precision. Prior/posterior tables treating all 58 as parameters
double-count. Pinned in the ETH run: alpha_2, kappa, mobility_omega, mobility_gamma, tau_i.
`zeta_2` IS drawn from its own prior then overwritten by zeta_1/zeta_ratio -- one wasted draw.

Caveat: the Weibull/Laplace tail model is fitted 300-5,000 nats from the optimum, so d_eff is a
tail index there, not necessarily the near-mode dimension. Confident in "astronomically large"
and in the noise-vs-theta ordering; NOT confident in the exponent.

Related: [[param-identifiability-eth-v0903]]
