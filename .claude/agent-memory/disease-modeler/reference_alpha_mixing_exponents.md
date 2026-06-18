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
- Both are in `parameters_global` (NOT per-location). make_priors_default.R:45-61.

**Three-way value discrepancy (all confirmed 2026-06-17):**
- priors_default: alpha_1 Beta(mode 0.25, CI 0.05-0.5); alpha_2 Beta(mode 0.5, CI 0.25-0.75)
- spec param table (04-model-description.Rmd:1582-1583): BOTH listed as 0.95
- laser engine default_parameters.json:207-208: alpha_1=0.27, alpha_2=0.5
The spec table 0.95/0.95 is a STALE placeholder, NOT literature-derived; it contradicts both the
prior and the engine default. The prior central values (0.27/0.5) are the defensible ones and match
the engine. Engine constraint: alpha_1 in (0,1], alpha_2 in [0,1] (params.py:446-449).

**Pinned values for Phase-1 per-country fits:** alpha_1 = 0.27 (Glass-style sub-linear mixing,
matches engine default & prior mode ~0.25), alpha_2 = 0.5 (intermediate density/frequency, prior
mode & engine default). Use point values, not the distributions.

**Identifiability:** alpha_1 is structurally confounded with beta_j0 (both scale the numerator:
beta * X^alpha_1) and only weakly identified from a single incidence series -> FIX per-country.
alpha_2 needs cross-country N variation (40 countries spanning OOM in population) to identify
density-vs-frequency -> not identifiable within one country -> FIX per-country.

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
