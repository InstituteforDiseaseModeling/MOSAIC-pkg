---
name: likelihood-overconcentration-invariants
description: Measured over-concentration of MOSAIC's daily-independent NB likelihood — VIF vs block length, tau, effective sample size, k-floor binding, and score Monte Carlo noise (v0.90.3)
metadata:
  type: reference
---

Durable numbers for MOSAIC's likelihood calibration. All measured on v0.90.3 against the ETH 25k
and 40-location 100k configs (`scratch/LIKE/` in `MOSAIC-pkg/claude/review_inference/`).

## Variance inflation grows LINEARLY with the scoring timescale
`VIF(B) = realised block error^2 / NB-asserted block variance`. Median over the 25 locations with
>=500 observed cases, at the best draw:

| block | 1 d | 7 d | 28 d | 91 d | 365 d |
|---|---|---|---|---|---|
| VIF cases | 2.4 | 13.8 | 44.5 | 101.9 | 252.2 |

Independent errors would give a FLAT VIF. Linear growth = residual correlation that has not decayed
at a year. Integrated autocorrelation time of `log((y+1)/(mu+1))`: median tau = **92** (cases),
**65** (deaths); ETH cases tau = 210-257, ACF still 0.27 at lag 180.

**Control:** against a 28-day moving-average "perfect" mean path, VIF = 0.9-1.5 at every B. The
inflation is **model misspecification**, not intrinsic surveillance noise.

## Trap: raw Pearson-residual ACF reads ~0 at every lag
20% of `sum(residual^2)` sits in the top 1% of cells; a spiky series has near-zero ACF. **Use the
log-ratio residual `log((y+1)/(mu+1))` or a rank ACF.** I nearly reported "residuals are
independent" from the Pearson ACF.

## Aggregation test — the cleanest statement of over-counting
Pooling ETH's 3,292 daily cells into **9 annual blocks** (dispersion k*B) retains **89%** of the
LL spread across draws. Weekly = 111%. The other ~3,283 daily terms add ~11%.
=> N_eff: ETH ~60 effective observations vs 58 sampled parameters (model effectively unidentified);
40-loc ~1,800 (upper bound, ignores spatial coupling).

## nb_k_min = 3 binds EVERYWHERE, not "28 of 29"
40-loc config, cases: **28/28** locations with a finite MoM estimate are floored; the other **12**
fall to `k = Inf` (Poisson). **Zero locations** have a data-set dispersion. Median k_raw = 0.123 =>
median floor inflation **24.4x** (max 188x). Deaths: 13/14 floored, 26 Poisson, 1 data-set (NER).
Spec (`05-model-calibration.Rmd:127-131`) specifies the **unfloored** MoM and a **VMR<1.5** Poisson
switch; code uses `v <= m` (VMR<=1) and a floor. Nuance: at the DAILY scale the residual-calibrated
k is 1.4-2.5 (cases), so k=3 is roughly right there — the floor's real sin is presenting the
*marginal* series dispersion as if it were *residual* dispersion.

## Monte Carlo noise of the score exceeds the gaps it ranks by
ETH, 12 draws x 24 fresh seeds: median seed-to-seed SD of a single-replicate LL = **231.3**; of the
3-replicate `calc_log_mean_exp` score = **120.4**. Recorded best-vs-2nd gap = 244 => z = 1.4,
p ~ 0.16 — **the best draw is not significantly better than the second**. The recorded score exceeds
its own 24-seed mean by 212 (max-of-3 selection bias). `log-mean-exp` is a pseudo-marginal estimator
of `log E_seed[L]`; it needs replicate log-SD ~O(1) (Doucet 2015) and we are at 231.

## Deflation alone does NOT fix ESS — quantified
Same 4,000 ETH sims. tau needed for ESS/n = 0.1: **2,839** (production), **543** (mean-floor +
unfloored k). Measured tau is 63-257. Best achievable stack (mean floor + k raw + tau=210) gives
ESS = 60/4,000 = **1.5%**, still 2.6x short. The residual gap is a PROPOSAL problem (prior draws
genuinely differ by ~170 calibrated log units), not a likelihood problem.

See [[likelihood-zero-penalty-dominates]] for the dominant term, and
[[weighting-stack-measured-v085]] for the downstream weighting.
