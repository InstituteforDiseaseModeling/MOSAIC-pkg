---
name: b2-cfr-chain-factor-diagnosis
description: B2 mu<-CFR_target derivation has a real chi-blend-vs-chi_eff error + needs (1-e^-g1) not g1, but residual ~2x offset is dynamics-dependent not a clean constant
metadata:
  type: project
---

The B2 derivation `mu_j_baseline = CFR_target * gamma_1 * rho / (rho_deaths * chi_blend)`
(sample_parameters.R B2 block ~L656; docs 04-model-description.Rmd L994-1007 "steady-state
identity") was diagnosed against the ACTUAL laser-cholera deaths mechanism (infectious.py).

**The engine's true reported-CFR identity** (verified by controlled single-location COD probes,
deterministic, mu-swept and gamma_1-swept):
  CFR_eng = mu * rho_deaths * chi_eff / (rho * (1 - e^{-gamma_1}))
- deaths are a FLOW from the Isym stock: disease_deaths = Isym * (1-e^-mu) ~= Isym*mu (verified ratio 1.006)
- reported_cases (infectious.py L143) reads the Isym STOCK each tick * rho/chi_eff, but summed it
  scales with INCIDENCE not prevalence-days -> the 1/(1-e^{-gamma_1}) dwell factor. eng/identity
  ratio is ~8.5 at gamma_1=0.2, and `eng/id * (1-e^-gamma_1)` collapses to ~1.5 (flatter than `*gamma_1`
  which gives 1.5->1.86). Using Isym-weighted effective chi (chi_ep-dominated in epidemic regime,
  0.83 not blend 0.68) the residual -> 0.984 ~= 1.0. So the closed-form identity IS exact per-config.

**Two genuine (principled) errors in B2 vs the engine identity:**
1. dwell: B2 uses `gamma_1`, engine uses `(1-e^{-gamma_1})`. ratio g1/(1-e^-g1) ~ 1.03-1.11 (small).
2. chi: B2 uses chi_blend = 0.5(chi_en+chi_ep); engine effectively uses chi_eff ~ chi_epidemic
   because reported_cases is dominated by epidemic-regime ticks. chi_ep/chi_blend ~ 1.2-1.4. THIS is
   the dominant principled factor and it is per-country (depends on chi_ep/chi_en spread).

**But the cal-doctor's measured realized/CFR_target is NOT uniform** (median 1.70, CV 26%, range
0.98-2.51 across deaths-rich set) and is NOT explained by a global kappa_engine=0.55 constant.
After applying the principled dwell+chi_ep correction the residual gets WORSE (CV 34%) with
cor(resid, chi_ep/blend) = -0.69 -> flat chi_epidemic over-corrects. The true chi_eff is the
per-country REALIZED epidemic-regime fraction (a simulation-dynamics quantity, NOT a prior-time
closed form), plus a multi-location/spatial residual (single-loc identity is exact; full-network
medoid adds ~1.4x unexplained).

**Verdict: a global kappa_engine ~ 0.55 fudge is WRONG** (masks a structured, per-country,
dynamics-dependent error). The principled fix is to replace `gamma_1 -> (1-e^{-gamma_1})` and
`chi_blend -> chi_eff` in the derivation, where chi_eff leans toward chi_epidemic. That removes the
derivable part; the remaining dynamics-dependent residual cannot be closed-form and argues for either
(a) keeping deaths weight low (currently 0.5) and accepting residual deaths bias, or (b) a calibrated
per-country empirical correction estimated from a deterministic pilot, NOT a global constant.

**Re-validation scope:** mu rescale is a CLEAN deterministic LINEAR lever on deaths
(engine reported-CFR exactly linear in mu, cases mu-INVARIANT: tot_rc flat across mu x1..x5;
weight_deaths=0.5, weight_cases=1, shape weights 0). So a chain-factor change can be validated by
deterministic sandbox checks + the 6-country dugong; a full 27-country re-run is NOT required to
verify the deaths-scale change (cases posterior ~unaffected). Sandbox at
MOSAIC-pkg/claude/diagnose_fit/engine_*_probe.R / engine_gamma1_test.R / engine_chi_decomp.R.
