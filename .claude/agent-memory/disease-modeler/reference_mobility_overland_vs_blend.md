---
name: mobility-overland-vs-blend
description: est_mobility air vs fused_raked (overland-only) vs blend at v0.101.0 - production is overland tau + blend kernel; blend ~= overland kernel (dgamma +0.054), switch immaterial; doc claims unsupported; MCMC SDs are pseudo-precision; SCI artifacts + fusion weights dominate gamma
metadata:
  type: reference
---

Measured 2026-10-02 on MOSAIC 0.101.0 / config v6.1 / priors v17.1 / MOSAIC-data 2ee3725,
great_circle metric (builders read the `_blend` suffix, no `_tt`). Scratch (laptop):
`MOSAIC-pkg/claude/mobility_overland_vs_blend/` (scripts 00-07, tables/, figures/).

**What ships (correct the common misstatement):** only gamma/omega come from
`est_mobility(od_source="blend")` (1.8997 / 0.6271; priors Gamma(4.80,2) / Gamma(2.25,2),
mode-matched). `tau_i` in BOTH config and prior median = `est_overland_tau_prior()`
(user decision 2026-09-18), NOT the blend tau. The fused_raked matrix is raked to those same
margins, so production = overland departure + blend kernel.

**Reproduction:** est_mobility is unseeded (JAGS chain seeds come from R `sample()`), so bitwise
reproduction is impossible; blend M rebuilt from current data is cell-identical to the archived
fit's M (tag `archive/hold-defaults-rebuild-2026-09-28`), and shipped values sit inside 10 seeded
refits (+/-1.2 MC sd). Overland tau file reproduces byte-exact. Air JAGS scores the fractional
OAG counts as ROUNDED integers (ML on raw fractions is 0.0055 low in gamma).

**Results (MCMC mean; origin-row bootstrap 95%):** air gamma 1.362 [1.11,1.63], omega 0.616;
fused_raked 1.954 [1.73,2.11], 0.628; blend 1.900 [1.67,2.06], 0.627. Paired
fused_raked-blend dgamma +0.054 [0.035,0.078], domega 0.001 (n.s.). Blend is 93% overland by flow
(overland 376k/day vs air 28k/day off-diagonal), so its kernel IS the overland kernel.

**Uncertainty:** MCMC sd (~0.003) is pseudo-precision - Pearson phi 146-341; honest sd gamma ~0.10,
omega ~0.06. Fusion weights move gamma far more than air does: no-SCI overland 2.41, flows+contig
2.51. Meta SCI supplies ~70% of the fused structure's >2000 km mass incl. artifacts (ZMB<->LBR is the
TOP SCI partner both ways in the RAW HDX country.csv: 1.12M vs ZMB-ZWE 0.13M; also TZA-NER,
NGA-BDI, NGA-SOM). Handed to data-engineer.

**Engine impact of blend->overland kernel:** arrivals x0.965-1.032 (ZAF/SWZ), 0/40 top-destination
changes, long-range seeding FOI ratio >=0.971; prior overlap 0.98 (gamma) / 0.999 (omega);
prior-predictive shift <=1.2% of the 90% width. Total travellers in any run are set by tau alone
(pi rows sum to 1 within the run's patch set). alpha_1=0.27 compresses imports: 10x travellers ->
1.86x FOI; even air (13x fewer travellers) cuts long-range seeding FOI only ~36%.

**est_mobility roxygen claims are NOT supported:** fused_raked also yields one tau + one
gamma/omega pair (config can represent it); the engine sees only the two exponents, and the raked
overland matrix already carries 54k/day on >2000 km links vs air 7.6k/day. Gravity kernel misfits
both empirical matrices (TV ~0.37/origin; contiguous share 53% kernel vs 68% data; >2000 km 21%
vs 15%) - kernel FAMILY and the prior width (P(omega>1)=0.48) matter more than the OD source.
Recommendation given: keep blend for v1.0; post-1.0 tighten gamma/omega prior (statistician),
audit SCI (data-engineer), fix doc claims. Related: [[tau-i-departure-semantics]],
[[diprete-2025-transmission-units]], [[weill-2017-waves-palette]].
