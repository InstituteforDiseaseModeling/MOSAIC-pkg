---
name: cfr-submodel-simplification-audit
description: 2026-09-23 adversarial audit of the whole CFR/mortality submodel — measured B2.1 error decomposition, the CFR_target sdlog=0.787 width defect, the WHO-annual-vs-scored-data centre mismatch, the degenerate epidemic_threshold, and the est_CFR_hierarchical Angola-leak bug
metadata:
  type: reference
---

Full report: `/Users/johngiles/MOSAIC/MOSAIC-pkg/claude/cfr_review/03_epi_review.md` (local laptop).
Measured at `priors_default` v15.18 / `config_default` v4.7 on 20 ETH prior-predictive draws through
the real `sample_parameters()` -> `run_simulation()` path.

## The exact identity (re-derived from `sim_components.R`, supersedes the approximate one)

```
implied reported CFR = CFR_clin * rho_deaths * chi_eff / rho
CFR_clin = p_mu / (p_mu + (1-p_mu) * p_g1)     <- competing risks, deaths drawn BEFORE recovery
```
B2.1 uses `CFR_clin ~= p_mu / p_g1`. Measured decomposition of `realized/CFR_target` (geomeans):

| factor | value |
|---|---|
| **(1 + eps*f_epi)** — NOT in the derivation | **1.3873** |
| slope factor | 1.0008 (sd 0.021) |
| chi_bar/chi_epi | 0.9822 |
| 1/k_ns (stationarity) | 0.9680 |
| competing-risks correction `1/(1+CFR*k)`, k=rho/(rho_d*chi_ep)=1.354 | 0.9675 |
| unexplained residual | 1.0167 |
| **total** | **1.2985** |

=> B2.1's four modelled chain factors are right to ~5% combined; the ONLY material error is the omitted
epidemic multiplier. Competing-risks omission is −2.6% at CFR 2% but **−15% at COG's 9.1%**.

## THREE NEW defects (not in any earlier note)

1. **`sdlog_cfr = 0.787` is uniform across all 40 countries and is backwards.** GAM's own per-country
   sdlog on the 2021-25 CFR: **0.015-0.091 for the 17 data-rich countries** (COD 0.0145, NGA 0.0153,
   ETH 0.0353) and **1.19-2.03 for the no-data ones** (GAB 2.03, GNQ 2.01, ERI 1.90, BWA 1.80). So the
   prior is 9-54x TOO WIDE where the CFR is measured and TOO NARROW where it is not. Its stated
   justification ("preserves the pre-B2 implied-CFR prior spread", SPEC_B2 §2.2) is compatibility with a
   DELETED parameterisation, not evidence. Consequence: prior p97.5 CFR = 10.5% COD, 42.5% COG, 40.3%
   CAF/MLI; and it is the mechanism by which best-subset selection manufactures deaths bias
   (p90/median = 2.74x at sdlog 0.787 vs 1.45x at 0.29).
   **Recommended: per-country `sdlog = clamp(sqrt(GAM_sdlog^2 + 0.261^2), 0.25, 0.60)`, truncate at 0.25.**

2. **The prior's anchor dataset is not the scored dataset.** `CFR_target` centre = WHO AFRO annual GAM;
   the likelihood scores `config_default$reported_cases/deaths` (multi-source weekly). Over the 17
   countries with >=50 observed deaths in the 2023-2027 fit window:
   **geomean(CFR_target / scored observed CFR) = 1.119, sd(log) = 0.261.** NGA 1.42x, SOM 2.31x,
   CIV 1.48x, GHA 1.46x, COG 1.41x, CAF 1.36x. Against WHO annual itself the centre is fine (1.045), so
   this is a dataset-mismatch, not a GAM error. The 0.261 is the right empirical width component.
   **BFA is broken: centre 0.0278% (clamped to the 0.2% floor) vs 4.37% observed = 22x low.**

3. **`epidemic_threshold` is derived as the MEDIAN weekly incidence over weeks with >=1 case**
   (`make_priors_default.R:1998-2060`) — by construction the 50th percentile of endemic+epidemic activity.
   Resulting triggers: **CIV 2.1 people**, LBR 5.4, BEN 6.5, RWA 8.3, NGA 212, ETH 538, COD 1,172 (SSD
   368 is the highest bar). Measured `f_epi = 0.88` death-weighted and `chi_bar/chi_epi = 0.982`.
   **Therefore `mu_j_epidemic_factor` is NOT an epidemic term — it is a constant multiplier fully
   confounded with `CFR_target`, and `chi_endemic` governs only ~5% of case weight.** The two-regime
   structure means nothing until the threshold is re-derived as an upper quantile (~p85).
   Corroborated by the ETH OAT profile: `chi_endemic` argmax at p98 (wants 0.96) and `epidemic_threshold`
   argmax at p02 (wants the floor) — the likelihood is asking for ONE high chi from two directions.

## A 4th, latent bug in `est_CFR_hierarchical()`

`R/est_CFR_hierarchical.R:325-330` predicts no-data countries with `exclude = "s(country_factor)"`, which
removes the random INTERCEPT but not the `s(year, country_factor, bs="fs")` factor smooth — so the
"population average" is actually **Angola's** trend (`model_countries[1]`). Re-fitted and measured:
**2.83-3.45x the true population average** (3.10% vs 1.10% in 2021). Currently affects **BWA only**
(39 of 40 ISOs clear the >=20-case filter), but it fires silently whenever a country drops out.

## Redundancy verdicts

- `mu_j_slope`: **inert.** deaths-weighted factor 1.0008 +/- 0.021; profiles at 113 nats = the engine RNG
  noise floor (70-148). Prior `N(0, 0.05)` is uncited AND 3x too narrow to express the GAM's own
  −7.6%/yr trend, which is anyway averaged out of `CFR_target` (a 2021-25 mean). Pin to 0.
- `rho` and `chi_*` enter the engine ONLY as `rho/chi_eff`; the same ratio appears in B2.1. Three
  parameters + a threshold encode one number.
- `rho_deaths`: cancels exactly (class D FLAT, 119 nats). Pin the draw, KEEP the prior as documentation —
  it is required to convert reported deaths to true deaths for any burden statement.
- Per country the deaths channel carries 7 free parameters against ~1 well-measured number (the level);
  14 of 40 countries have ZERO observed deaths, i.e. 33-42 draws informed by nothing.

## chi-convention inconsistency (3 places, pick one)

B2.1 uses `chi_epidemic`; the `epidemic_threshold` conversion uses `chi_endemic` (making the threshold
~31% lower); the IC moment-match (`sample_parameters.R:1001`) uses `chi_endemic` (E/I seeds ~31% low).

## Recommended order (all recalibration-gated)

1. Re-centre + per-country re-width `CFR_target` (prior-only, highest value-per-risk, nobody proposed it).
2. Pin `mu_j_slope=0` and `rho_deaths=0.4194` (−41 draws, provably inert).
3. Re-derive `epidemic_threshold` as ~p85 — until then eps is unidentifiable IN PRINCIPLE.
4. B2.2, after A1b. 5. `Gamma(1,8)` on eps for correctness only (no extra bias gain on top of B2.2).

**Expect bias 2.0-2.9x -> ~1.2-1.5x, cases untouched, and NO R2 gain on either channel** — the deaths R2
ceiling is timing/zero-inflation (see [[deaths-channel-limits]]), not CFR.

Related: [[cfr-mu-j0-identity]], [[project-prod-deaths-bias-b2-epi-gap]], [[mu-j-epidemic-factor-prior]],
[[param-identifiability-eth-v0903]], [[likelihood-noise-floor]], [[cfr-target-prior-tail]]
