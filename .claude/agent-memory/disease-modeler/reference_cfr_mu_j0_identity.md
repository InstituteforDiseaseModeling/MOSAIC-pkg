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

---

## UPDATE 2026-09-19 — three corrections, measured (CFR-MATH track)

Evidence: `MOSAIC-pkg/claude/inference_lab/reports/CFR-MATH.md`.

**(a) The identity above is the PRE-B2 form and is now stale.** Shipped (B2.1, v15.15 / v0.50.0,
`R/sample_parameters.R:671-674`):
`mu_j0 = CFR_target * (1 - exp(-gamma_1)) * rho / (rho_deaths * chi_epidemic)`
— incidence dwell `(1-e^{-gamma_1})`, NOT `gamma_1`; `chi_epidemic`, NOT the blend. `mu_j_baseline`
is DERIVED at sample time; there is no `mu_j_baseline` location prior any more (the
`Gamma(4, rate)` per-country prior described above, and still in the spec at :1007, is gone).
Verified: the derivation reproduces the stored `mu_j_baseline` to 3.4e-15 over 25,000 draws.

**(b) `rho_deaths` cancels from the ENTIRE DISTRIBUTION of reported deaths, not just the mean.**
Binomial-thinning composition gives, conditional on Isym,
`reported_deaths[t] ~ Binom(Isym[t-l_d], (1 - e^{-mu}) * rho_deaths)`, and under B2.1
`mu = c/rho_deaths` with `c = CFR_target*(1-e^{-gamma_1})*rho/chi_epidemic` (rho_deaths-free), so
`(1-e^{-c/rho_d})*rho_d = c - c^2/(2 rho_d) + ...` — leading term exactly rho_deaths-free,
residual **0.11%** at ETH prior centres. `rho_deaths` has exactly ONE engine use site
(`R/sim_components.R:204`). Measured sweep at fixed CFR_target with mu re-derived:
**3.99% variation in reported deaths over rho_deaths 0.25-0.85 (a 3.4x range); 1.7% over the
prior 95% CI [0.319, 0.524]; 0.24% on cases.** The only real effect is second-order Isym depletion
and it lands on the CASES channel.

=> **The paragraph above ("the deaths likelihood identifies the PRODUCT mu_j0 * rho_deaths ... a
narrow rho_deaths prior pins it") is PRE-B2 and no longer true.** So is the identical text at
`04-model-description.Rmd:970` and `make_priors_default.R:592-596` — fix both. **PIN rho_deaths**
(`sample_rho_deaths = FALSE`, hold at the Beta mean 0.4194); the prior stays documented as a
statement about the true-vs-reported death ratio for interpretation, not as an identifiability aid.

> **DONE 2026-09-23 (CFR restructure R2, priors_default v15.19).** Pinned at 0.42 in
> `R/sample_parameters.R:178` + `R/run_MOSAIC.R:3566`; the pre-B2 "product" rationale is retired in
> `make_priors_default.R` and `04-model-description.Rmd`. Falsifiable 4-arm 48-seed gate executed —
> see [[r2-pins-rho-deaths-delta]] for the numbers and the reusable arm design.

**(c) The realized reported CFR is NOT CFR_target.** Exact closed form:
```
realized_CFR / CFR_target = (1 + eps*f_epi) x (chi_eff_bar/chi_epi) x (1/k_ns) x k_dd x (1/k_rc)
```
(f_epi = death-weighted epidemic-tick fraction; k_ns = sum(new_sym)/((1-e^{-g1})*sum(Isym));
k_rc = the `round(drawn/chi_eff)` low-count loss). Reproduces the simulated realized CFR to a
**median 2.8%** over 400 ETH draws. ETH posterior medians: `(1+eps*f_epi)` **1.380**, and the
product of the other four **1.000** — i.e. the whole deviation is the omitted epidemic factor.
Production median across 19 countries: **1.444**. The spec's "irreducible ~1.3-1.5x
dynamics-dependent residual" (`04-model-description.Rmd:1007`) is NOT irreducible; see
[[project-prod-deaths-bias-b2-epi-gap]] and [[deaths-channel-limits]].

**(d) `cfr_clinical_*` in `R/calc_implied_cfr.R:190` uses the wrong form.** It computes
`1 - exp(-mu_eff/gamma_1)`; the engine's competing-risks per-episode CFR (deaths drawn first,
then recovery on survivors) is `mu_eff / (mu_eff + (1 - e^{-gamma_1}))`. The shipped form crosses
0.5 at `mu/gamma_1 = 0.693` where the true value is 0.41, and the comment at :209-212 states the
threshold as `mu/gamma_1 = 1`. It also uses the continuous rate where the engine uses the per-tick
probability — the same distinction the v0.88.1 fix made two lines earlier for the surveillance CFR.
Fixing it halves the "biologically extreme" warning rate (MLI 5.81% -> 2.83%).
