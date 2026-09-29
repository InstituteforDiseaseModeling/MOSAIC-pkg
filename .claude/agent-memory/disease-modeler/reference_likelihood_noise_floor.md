---
name: likelihood-noise-floor
description: Engine RNG makes sd(log Lhat)=70-148 nats at fixed theta; 115 replicates of ONE parameter vector give exact-IS ESS 3-13, so ESS=1 is NOT (only) a proposal problem and no prior/proposal fix works until the score noise is fixed
metadata:
  type: reference
---

Measured on ETH x 25,000 (`/Users/johngiles/MOSAIC/output/eth25k_v0903/`, MOSAIC v0.90.3,
local laptop), 2026-09-17. Scripts: `MOSAIC-pkg/claude/review_inference/scratch/SAMP/`.

## The number
Score the SAME parameter vector at many fresh engine seeds:

| draw rank | sd(single-iteration logL) | observed range | sd(reported 3-iter LME score) |
|---|---|---|---|
| 1 | 128 | 527 nats | 70 |
| 10 | 233 | 1,356 | 149 |
| 115 | 197 | 901 | 139 |
| 2000 | 95 | 404 | 74 |

Median sd(log Lhat) at the production `n_iterations = 3` is **137 nats**.

## Why it is decisive
- 115 replicates of a **single, identical** theta, scored exactly as production does, give
  exact IS **ESS = 3-13** (not 115). At n_iterations=1 and 60 replicates, ESS = 1.0-1.96.
  **A perfect proposal would still give ESS ~ 1.**
- D2 decomposes: `D2_total = D2_theta + D2_noise`. `D2_theta ~ 21 nats` (43 free dims);
  `D2_noise = sd(log Lhat)^2 ~ 1.9e4 nats`. The noise term dominates by ~3 orders of magnitude.
- Winner's curse: the recorded max logL is **+110 nats above its own expected score** (measured
  at rank 1; a MC simulation at sigma=140 predicts +92). Exponentiated, that is a weight
  inflated by ~e^110.
- More internal iterations do NOT fix it: bootstrapped `sd(log Lhat)` falls only as
  `M^-0.13` to `M^-0.63` (log-mean-exp of dispersed lognormals is max-dominated), so reaching
  the pseudo-marginal-efficient sd ~ 1.2 (Doucet et al. 2015 Biometrika 102:295) needs
  M ~ 1e3-1e17 engine runs PER DRAW.

## What is NOT broken
Ranking is robust: two independent re-scorings at sigma=140 share **92%** of the top-115 and
the modal argmax wins 81% of the time. So a **rank/tolerance-based selector tolerates this
noise; an exponential-weight estimator does not.** Do NOT "just switch to exact IS weights" --
the saturated dAIC-4 weighting is inadvertently shielding the pipeline from a score it cannot
exponentiate. Fix the noise first.

## Mechanism (my lane)
`calc_model_likelihood.R:245-250` estimates the NB dispersion k by method-of-moments **from the
observed data only** (`.nb_size_from_obs_weighted(obs_c, ...)`), so it never contains the
simulator's trajectory-to-trajectory (process) variance. Each free-running simulated trajectory
is scored as if it were the conditional mean. Fix direction: make the observation model
represent `Var(obs | theta) = observation variance + process variance` (synthetic/composite
likelihood), or marginalise trajectories with a particle filter. Estimator choice -> statistician.

Related: [[required-n-prior-as-proposal]], [[param-identifiability-eth-v0903]]
