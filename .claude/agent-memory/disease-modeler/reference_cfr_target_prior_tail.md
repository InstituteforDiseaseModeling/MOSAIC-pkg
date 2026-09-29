---
name: cfr-target-prior-tail
description: The CFR_target lognormal (sdlog 0.787) puts 5-6% of MLI/COG draws above a 30% national reported CFR; MLI/COG WHO-GAM centres rest on ~90 effective cases. The production "cfr_clinical_epidemic > 0.5" warnings are a pure prior-predictive artifact, reproduced to 3% by Monte Carlo.
metadata:
  type: reference
---

Measured 2026-09-19. Report: `MOSAIC-pkg/claude/inference_lab/reports/CFR-MATH.md` (CFR-MATH-07).

## The warnings are prior-predictive, not posterior

The 100k production run flagged `cfr_clinical_epidemic > 0.5` on **MLI 5,893/100,000** and
**TCD 1,506/100,000**. Monte Carlo from the priors alone (n=1e5 draws of gamma_1/rho/rho_deaths/
chi_epidemic/eps/CFR_target, no likelihood) predicts **MLI 6.05%** and **TCD 1.548%**. Reproduced
to within 3%. It is NOT a corner of parameter space the data selected, and NOT an inference bug.

## The driver is CFR_target itself

Among MLI draws with `cfr_clinical_epidemic > 0.5`, conditional-vs-overall means:
`CFR_target` 0.385 vs 0.121 (**ratio 3.17**), `chi_epidemic` 0.86, `rho` 1.20, `1+eps` 1.09,
`gamma_1` 0.97, `rho_deaths` 0.97. **60.2% of flagged draws have `CFR_target > 0.30`; 20.7% have
`CFR_target > 0.50`.** A *reported* (suspected-case) CFR above 30% sustained at national-year
scale has never been observed in cholera surveillance.

## Provenance: MLI and COG rest on ~90 effective cases

`model/input/param_mu_disease_mortality.csv`: MLI hierarchical-GAM CFR 8.80% (2023) / 8.80%
(2024) / 8.65% (2025) with Beta shapes (9.98, 103.4) / (8.52, 88.3) / (7.31, 77.2) — an effective
sample of **~85-113 cases**. The raw weekly surveillance record for MLI 2014-2026 contains **zero
cases and zero deaths** (374 week-rows). COG (median 8.53%) is comparable. TCD (5.07%) at least
has 5,044 cases / 199 deaths behind it. The GLOBAL `sdlog = 0.787` is then applied on top of every
centre regardless of how much data is under it.

`CFR_target` prior medians (top): MLI 0.0892, COG 0.0853, CIV 0.0572, TCD 0.0507, TGO 0.0373,
ZAF 0.0372. p97.5 at sdlog 0.787: MLI 0.417, COG 0.399, CIV 0.268, TCD 0.237.

## Fixes

1. **PRIOR: truncate the `CFR_target` lognormal at 0.25.** ~3x the highest credible national
   reported CFR (TCD's WHO-GAM 5.5-5.9%, COG's raw 6.4%), well above every prior median, so it is
   INERT for 37 of 40 countries and removes 6.2% / 5.5% / 1.5% of MLI / COG / TCD draws.
2. **PRIOR BUILD: warn when a country's CFR Beta ESS (shape1+shape2) is below ~200.** Makes the
   MLI/COG provenance visible instead of silent. A per-country `sdlog` as a function of the GAM
   ESS is the better answer; sizing -> statistician.
3. **IDENTITY: the diagnostic formula is also wrong** and amplifies the flag rate ~2.06x — see
   [[cfr-mu-j0-identity]] update (d). Counterfactual flag rates for MLI: shipped 5.81%;
   exact competing-risks 2.83%; B2.2 + shipped 2.09%; **B2.2 + exact 0.89%**.

**Verdict: prior problem (dominant) + identity problem (a 2x amplifier). Not a real corner.**

Related: [[project-prod-deaths-bias-b2-epi-gap]], [[deaths-channel-limits]].
