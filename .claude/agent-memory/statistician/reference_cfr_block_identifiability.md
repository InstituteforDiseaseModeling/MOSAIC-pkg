---
name: cfr-block-identifiability
description: The mortality block is rank 3 of 4 (rho_deaths cancels EXACTLY under B2), mu_j_slope needs ~74k deaths, mu_jepi separability is omega=sqrt(duty cycle), and the deaths bias is a Jensen/eps artefact not a CFR-parameterisation problem
metadata:
  type: reference
---

Measured 2026-09-23 on `/Users/johngiles/MOSAIC/output/ETH_prod093_50k` (50k draws, ETH,
a **v0.93.0** run from branch `feature/nb-dispersion-glmnb`) plus direct `run_simulation()`
profiles. Scripts: `MOSAIC-pkg/claude/cfr_review/s1..s12_*.R`; report `04_identifiability.md`.

**1. `rho_deaths` is EXACTLY unidentified, not "weakly identified".** B2 derives
`mu_j0 = CFR_target*(1-e^-g1)*rho/(rho_d*chi_ep)`, and the deaths mean is `rho_d * mu_j0 * Isym`,
so `rho_d` cancels **identically** (verified to machine precision on 50k draws; a 2.6x sweep with
re-derivation moves total deaths 0.9% and `ll_deaths` 55 vs a seed SE of 34). It appears nowhere
in the cases channel. `04-model-description.Rmd` L970 ("narrow prior keeps the product pinned")
is pre-B2 and now wrong — there is nothing to pin. Same file L1020 still quotes
`mu_jepi ~ Gamma(1,2)`; shipped is Gamma(3,6) since priors v15.18.

**2. Closed forms worth reusing.**
- `se(log deaths level) ~ sqrt(VIF / D)`, D = total observed deaths in the scored window
  (reproduces the full Fisher computation to 9%). ETH VIF from NB Pearson-residual ACF = **7.2**
  (weekly-aggregated 10.4; raw-obs ACF 31.7 is an upper bound that double-counts signal).
- `mu_jepi` vs level: information-weighted correlation is **omega = sqrt(f)** exactly, f = the
  info-weighted epidemic-tick fraction (ETH 0.746 -> omega 0.8638 measured). VIF = 1/(1-f).
  Curvature ratio `I_ee/I_LL = f(1-f)/(1+e)^2`, so eps can NEVER carry >1/4 of the level's
  information. Identified iff `f(1-f) D / VIF >= (1+e)^2 / sd_prior^2`.
- `se(mu_j_slope) = sqrt(VIF / (Var_v(tau) * D))`, `Var_v(tau)` = info-weighted variance of
  t/nticks (ETH **0.0387** vs 0.0833 uniform — deaths sit in a narrow band).

**3. Budget.** Deaths needed for posterior shrinkage 0.5: **level 12, mu_jepi ~620-1000,
mu_j_slope ~74,000**. Largest shipped series is COD at 4,139; all 40 countries pooled = ~15,600.
`p_eff = tr(I(I+I_prior)^-1)`: ETH **1.2-2.0 of 4**; summed over 40 locations **28.6 vs 160
carried** (120 sampled + 40 epidemic_threshold). **14 of 40 locations have ZERO deaths.**

**4. Cases:deaths ratios — three different numbers, only one matters.**
level |LLc|/|LLd| = **3.2** (deaths 23.7% of |LL|); **variance** sd 57,021 vs 2,335 = 24:1 in SD,
**596:1 in variance**; `cov(LLd, LL)/var(LL) = 0.005`. Selection is 99.5% cases. The "15-210x"
figure is unreproducible as a level ratio and an *under*estimate as a variance ratio.

**5. THE DEATHS BIAS IS A SCORING-RULE ARTEFACT (Jensen), not a CFR problem.**
The NB scale-MLE on the MEAN path is unbiased: `sum (y-cm)/(k+cm)=0` gives implied bias
0.999-1.034 for all `k` in [0.5, Inf). But production scores ONE stochastic realisation, whose
structural zeros are floored at `eps_j = 0.02*mean(obs)` (= 0.0079 deaths/day at ETH, costing
-6.10 per observed death). `E_seed[LL]` therefore peaks at a large over-prediction:
- eps sweep -> bias at the LL optimum: 0.0079 **2.60** / 0.02 2.02 / 0.10 1.62 / **0.20 1.04** /
  0.396 0.73. So **eps ~ 0.5*mean(obs)** de-biases the deaths channel.
- replicate averaging (score `rowMeans` of n sims) -> n=1 **2.59** / 3 1.61 / 8 1.32 / 24 **1.02**.
- under v0.91.14's `-y*log(1e6)` the same optimum is at bias **4.24**.
**Raising `weight_deaths` moves the bias the WRONG WAY:** top-40 of 200 random draws by
cases-only LL has deaths bias 1.65, by total LL 1.93, by deaths-only LL **2.48**. Per-channel
normalisation would make deaths over-prediction worse. Do not recommend it.

**6. dAIC-4 cap is a 290x truncation** (best subset n=118 spans a TRUE dAIC of 1,160; 99.15% sit
at the cap; max/min weight = e^2 = 7.389; run's own exact IS ESS = 1.02). But **selection** of
which 118 uses the untruncated LL, so the cap is NOT what makes the deaths channel inert — item 4
is. Reparameterisation *can* change the posterior; it just cannot fix the level.
