---
name: b2-cfr-chain-factor-diagnosis
description: B2 mu<-CFR_target chain is ALREADY CORRECT at laser 0.16.1; deaths over-prediction is CFR_target posterior drift (NGA/COD/ETH) + mu_j_epidemic_factor inflation (MOZ), NOT a derivation-form bug
metadata:
  type: project
---

The B2 derivation (sample_parameters.R ~L659-690, make_priors_default.R ~L1726):
  mu_j_baseline = CFR_target * (1 - exp(-gamma_1)) * rho / (rho_deaths * chi_epidemic)

**RE-VERIFIED deterministically against laser-cholera 0.16.1** (2026-06-26, full-metapop NMME
medoids NGA/COD/MOZ via run_LASER + run_fit_sandbox; probes in MOSAIC-pkg/claude/diagnose_fit/
b2_engine_cfr_probe.R). Engine deaths mechanism (infectious.py): disease_deaths = Binom(Is_next,
1-e^{-mu_jt}) drawn from the Isym stock AFTER natural-death but BEFORE recovery removal, where
mu_jt = mu_j_baseline*(1+mu_j_slope*t)*(1+mu_j_epidemic_factor*epidemic_flag); reported_cases reads
the Isym STOCK * rho/chi_eff each tick.

**FINDING — the gamma_1 dwell factor is ALREADY CORRECT.** gamma_1 sweep at fixed mu (NGA medoid):
realized reported-CFR / CFR_target is FLAT at ~1.22 across gamma_1 0.06->0.20 (deaths move 2.2x,
reported_cases gamma_1-INVARIANT ~2%). Solving D_engine = mu*rhod*chi_ep/(realCFR*rho),
D_engine/(1-e^{-g1}) ~ 0.81-0.87 (nearly constant) vs D_engine/g1 0.72-0.85 (drifts more). So
(1-e^{-gamma_1}) is the right dwell; my earlier "needs the dwell" note is SATISFIED by current B2.
mu is exactly linear in deaths, cases mu-INVARIANT (re-confirmed). The earlier 0.16.1 "coupling
re-emerged" read was a MISDIAGNOSIS: gamma_1 is handled; the bias is two OTHER things.

**THE ACTUAL TWO CAUSES (decomposed by setting mu_j_epidemic_factor=0 and by re-deriving mu from
the PRIOR-median CFR_target):**
1. CFR_target POSTERIOR DRIFT (dominant for NGA/COD/ETH). Medoid CFR_target sits 2.1-3.5x ABOVE
   its prior median, and the prior is correctly observed-anchored (NGA prior 0.0276 vs obs 0.0275;
   COD 0.0201 vs 0.0190; MOZ 0.0044 vs 0.0048; ETH 0.0121 vs 0.0126 — ALL ~1.0). Re-deriving mu
   from the PRIOR CFR_target collapses bias_d: NGA 2.14->0.98, COD 1.99->1.01 (cases bc flat to
   3dp). This is a CFR_target IDENTIFIABILITY problem (calibration pulls CFR_target up), NOT a mu
   form problem -> escalate to disease-modeler / weighting, not fixable in the derivation.
2. mu_j_epidemic_factor INFLATION (dominant for MOZ). Engine multiplies mu by (1+epi_fac) on
   epidemic-flagged ticks; mu_epi_factor=0 drops MOZ bias_d 2.80->1.23 (NGA 2.14->1.60, COD inert).
   The death-weighted epidemic-tick fraction f=(infl-1)/fac is NGA 0.98 / COD 0.01 / MOZ 0.78 — a
   pure simulation-dynamics quantity, range 0->1, NO closed form predicts it from mu_epi_factor
   ((1+fac), sqrt(1+fac), 1+0.5fac all fail). NOT derivable at sample time.

**VERDICT: do NOT add a gamma_1-conditional re-derivation and do NOT apply per-country mu scalars.**
The doctor's NGA x0.45 / COD x0.50 / MOZ x0.35 are NOT gamma_1 effects — they are absorbing the
CFR_target drift + epidemic inflation, i.e. exactly the "flat scalar that keeps regressing" trap.
The B2 chain is sound and cases-neutral; leave the derivation form unchanged. Real fixes live
elsewhere (CFR_target prior tightening/identifiability; optionally a mu_j_epidemic_factor prior
that is less death-inflating, or scoring on a deaths weight that doesn't let CFR_target absorb shape).

bias_d != realized/CFR_target: bias_d = predCFR/OBSERVED-CFR = (CFR_target_drift) x (chain
realized/target). Always decompose the two.
