---
name: alpha-mixing-exponents
description: FOI mixing exponents alpha_1 (numerator, infectious) and alpha_2 (denominator, density-vs-frequency); spec-vs-prior discrepancy resolution and fix/free verdict
metadata:
  type: reference
---

FOI (spec 04-model-description.Rmd:90-94, engine humantohuman.py:82-92):
lambda = beta_jt^hum * (1-tau)S * [ (1-tau)(I1+I2) + sum_i pi_ij tau_i I_i ]^alpha_1 / N^alpha_2

- **alpha_1** = exponent on the infectious bracket (numerator). S is OUTSIDE the power. 1 = mass-action/well-mixed; <1 = saturating/heterogeneous contact. Anchor: Glass et al 2003.
- **alpha_2** = exponent on N (denominator). 0 = density-dependent, 1 = frequency-dependent. Anchor: McCallum et al 2001.
- **alpha_2** is in `parameters_global`. **alpha_1 is PER-LOCATION** since priors v15.16
  (2026-06-24): `parameters_location$alpha_1$location[[iso]]`, a shared marginal
  Beta(28.4, 71.6) (mean 0.284) for all 40 ISOs. (This file said "both global" until 2026-09-18.)

**Three-way value discrepancy (all confirmed 2026-06-17):**
- priors_default: alpha_1 Beta(mode 0.25, CI 0.05-0.5); alpha_2 Beta(mode 0.5, CI 0.25-0.75)
- spec param table (04-model-description.Rmd:1618-1619): 0.27 / 0.50 -- CORRECTED since this
  note was written; the old 0.95/0.95 placeholder is gone and the table now matches the engine
- laser engine default_parameters.json:207-208: alpha_1=0.27, alpha_2=0.5
(Historical: the spec table once carried a 0.95/0.95 placeholder that was NOT literature-derived and
contradicted both the prior and the engine. That is RESOLVED -- the table now reads 0.27/0.50.) The
prior central values (0.27/0.5) are the defensible ones and match the engine. Engine constraint: alpha_1 in (0,1], alpha_2 in [0,1] (params.py:446-449).

**Pinned values for Phase-1 per-country fits:** alpha_1 = 0.27 (Glass-style sub-linear mixing,
matches engine default & prior mode ~0.25), alpha_2 = 0.5 (intermediate density/frequency, prior
mode & engine default). Use point values, not the distributions.

**Identifiability:** alpha_1 is structurally confounded with beta_j0 (both scale the numerator:
beta * X^alpha_1) and only weakly identified from a single incidence series -> FIX per-country.
alpha_2 needs cross-country N variation (40 countries spanning OOM in population) to identify
density-vs-frequency -> not identifiable within one country -> FIX per-country.

**STATUS 2026-09-18: the Phase-2 recommendation below is now the PACKAGE DEFAULT.**
`sample_alpha_1 = FALSE` and `sample_alpha_2 = FALSE` in BOTH default sites
(`mosaic_control_defaults()$sampling`, `default_sample_args`) as of v0.91.13; pinned by
`test-alpha1-pinned-default.R`. Until then production ran 40 FREE per-location alpha_1
(run `stage3_continental_b21_a1loc`) because only one default site had been consulted --
the posterior moved 0.057 prior SD vs a 0.146 null, i.e. it learned nothing. The VALUE stays
0.27: country-scale patches are weakly-coupled aggregates, so low mixing is intended; the
0.90-0.98 literature values are community/city-scale measles and not the comparison class.

**Verdict (recommended to user 2026-06-17):** FIX both alpha's in Phase 1. In Phase 2 (40-country
spatial fit), recommend KEEP BOTH PINNED rather than free them. Cross-country N variation gives
some alpha_2 signal in principle, but freeing global exponents while simultaneously estimating the
spatial network (tau_i, pi_ij) creates a sloppy ridge: alpha_1 trades off against every beta_j0 and
against the network coupling. Pinning preserves comparability of per-country beta posteriors. If
freed at all, free ONLY alpha_2 (one shared scalar) with a tight prior, never alpha_1.

**Also fix per-country (global + weakly identified from incidence alone):** kappa (infectious dose,
half-saturation - confounded with beta_env/zeta), zeta_1/zeta_2 (shedding - confounded with
kappa/decay), gamma_2 & sigma (asymptomatic arm - no direct asymptomatic observations). These are
global biological constants; per-country incidence carries little independent signal. Let per-country
fits move beta_j0_tot, mu_j_baseline, seasonality, and initial conditions instead.
