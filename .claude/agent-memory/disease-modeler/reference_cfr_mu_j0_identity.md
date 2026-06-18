---
name: cfr-mu-j0-identity
description: The mu_j0 / reported-CFR derivation identity (laser-cholera v0.13+) and what the deaths likelihood actually identifies
metadata:
  type: reference
---

Canonical source: `MOSAIC-docs/04-model-description.Rmd` §`{#case-fatality-rate}` (~lines 981-1017)
and the rho_deaths derivation (~line 970).

**Identity (v0.13+ engine):**
`mu_j0 = CFR_reported_j * rho / (rho_deaths * chi)`
- Derived from `CFR_reported = E[reported_deaths]/E[reported_cases] = mu_jt * rho_deaths * chi / rho`.
- sigma (symptomatic fraction) cancels: both observation pathways start from I_1, not infections.
- Per-country prior: Gamma(shape=4, rate) with CV=50%, moment-matched from a hierarchical-GAM
  reported CFR (binomial(deaths,cases) ~ s(year) + country RE) on WHO AFRO 2014-2025.
- Example: AGO reported CFR ~2.3% -> mu_AGO,0 ~ Gamma(4, 173), mean ~0.023/day -> ~11% integrated
  symptomatic-period CFR over a 5-day infectious period (1 - e^{-0.023*5}).

**What the deaths likelihood identifies:** the PRODUCT `mu_j0 * rho_deaths`, NOT the two factors
separately. The informative rho_deaths prior Beta(36.95, 51.02) (pooled SSA mean 0.42, from Routh
2017 TZA, Shikanga 2009 KEN, Bwire 2013 UGA; DerSimonian-Laird) deliberately pins rho_deaths near
0.42 so the per-country mu_j0 posterior carries the cross-country CFR signal cleanly. A wider
rho_deaths prior would smear identifiability between the two.

**Implication for ensemble/geometry choices:** because deaths identify a product and the
deaths-vs-N relationship is under-identified, any change that shifts which parameter sets dominate
the ensemble (e.g. best-subset geometry A_best/CVw_best) can move the implied-CFR (mu_j0*rho_deaths)
without moving the case fit. See [[best-subset-geometry-epi-review]].

**Engine semantics:** mu_jt is a per-day mortality hazard among symptomatic (NOT a per-infection
CFR); per-day death-transition prob = 1 - e^{-mu_jt}. Decomposed mu_jt = mu_j0 * mu_j1^... *
mu_j,epi (baseline x linear time-trend x epidemic-surge factor). mu_j_baseline already v0.13-
corrected in priors_default v15.6 (rho_deaths factor ~2.36x baked in) — do NOT re-adjust.
