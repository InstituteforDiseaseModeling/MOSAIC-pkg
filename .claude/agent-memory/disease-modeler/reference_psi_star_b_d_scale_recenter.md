---
name: psi-star-b-d-scale-recenter
description: psi_star_b prior re-centered 0->+1.0 for the new per-capita D-scale suitability psi (priors_default v15.11); transform is odds-multiply at the prior center
metadata:
  type: reference
---

The `psi_star_b` DEFAULT prior was re-centered mean 0 -> **+1.0** (sd kept 2.5) in
`priors_default` **v15.11** (make_priors_default.R ~line 1969) when MOSAIC switched its
default suitability psi from `transmission_intensity` to per-capita
`target_D_rate_per_country_floored` ("D", model/input/pred_psi_suitability_day.csv).

**The transform** (`R/calc_psi_star.R:114`): `psi* = plogis(a*qlogis(psi)+b)` i.e.
psi* = sigma(a*logit(psi)+b). At the prior CENTER a=1 it is an **odds-multiply**
psi* = sigma(logit(psi)+b), so a uniform positive b raises the LEVEL while preserving the
seasonal dynamic-range/shape on the logit scale (peaks stay peaks). b=0 at a=1 is the identity.

**Why +1.0:** D's psi sits much lower than the old scale (per-country mean COD 0.93->0.43,
KEN 0.25, MOZ 0.16, SOM 0.23; global all-country mean 0.14->0.10; pooled median ~0.012).
At the old b=0 center every country would START psi* at its near-floor raw D level (weak
environmental signal). +1.0 restores forcing comparable to the old scale for mid/high-burden
countries (at a=1: COD 0.43->0.67, SOM 0.23->0.45, MOZ 0.16->0.34, RWA 0.10->0.24) and lifts
low-burden off the floor (~0.07->0.17), WITHOUT saturating peaks. A larger "median->0.5"
recenter (b~=4.6) was REJECTED — it crushes dynamic range and saturates COD peaks to ~1.

This is a better STARTING center, not a constraint: sd kept 2.5 (95% CI [-3.90, 5.90]) so
calibration retains both-direction per-country freedom. D validation calibrations already fit
with the old b=0 because b is sampled.

**MOZ override removed:** the old MOZ-specific psi_star_b (mean +0.4, from calibration_test_19)
was fit on the OLD high-level scale and is not transferable; +1.0 already exceeds it, so it was
folded into the general center. NOTE the MOZ `psi_star_k` override (mean=-5) was LEFT in place
(time-offset, scale-invariant).

**psi_star_a unchanged** (Truncnorm(1,1,0,Inf)): a=1 identity is scale-invariant; the input-level
change does not alter what a means. Only b (the offset) needed re-centering.

**CRITICAL channel asymmetry (red-team 2026-06-19):** psi* feeds TWO engine channels with
OPPOSITE scale-sensitivity, and the v15.11 rationale only addressed one (the wrong one):
- **beta_env (envtohuman.py:24):** `beta_jt_env = beta_j0_env * (1 + (psi - psi_bar)/psi_bar)`
  = beta_j0_env * (psi*/psi_bar*). This is SELF-NORMALIZED per patch over time. For the low-psi
  D regime (psi<<1), the odds-multiply approx psi* ~= e^b * psi, so b CANCELS in the ratio
  psi*/psi_bar*. The "+1.0 restores forcing level" rationale is therefore largely a NO-OP for
  beta_env on low/mid-burden countries (only bites near saturation). beta_j0_env already carries
  the level. So the stated justification targets a channel that is ~scale-invariant.
- **delta_jt decay (environmental.py:33,150):** `delta = 1/(days_short + pbeta(psi*|s1,s2)*spread)`.
  This reads psi* on its ABSOLUTE level (NOT normalized). Here +1.0 genuinely LENGTHENS reservoir
  survival: low-burden psi 0.05->0.17 => pbeta(.|3,3) 0.003->0.07 => survival ~16->29d; COD
  0.43->0.67 => 0.80 pbeta => ~160d. This is a real, uncited biological change to the environmental
  reservoir half-life that the rationale never mentions. decay_days_*/decay_shape_* are GLOBAL +
  sampled, so calibration can absorb some of it, but not per-country.
TAKEAWAY: +1.0 is defensible as a starting center but its DERIVATION is mis-attributed — the level
matters for delta (reservoir survival), not beta_env. Should be re-derived/justified on the delta
channel, ideally validated against the 16-196d V.cholerae survival anchor (eq:decay-priors).

**RESOLVED (B4 fix, 2026-06-19):** make_priors_default.R psi_star_b comment + the v15.11 changelog
string RE-GROUNDED to the delta channel (beta_env self-normalizes so b ~no-op; delta reads absolute
psi* level). Validation script claude/validate_psi_star_b_delta_survival.R (+_RESULT.txt) computes
post-shift per-country survival = 16 + pbeta(psi*|3,3)*180 over the actual D-psi series at decay
prior MEANS: ALL 40 countries IN-ENVELOPE [16,196] (0 ceiling/floor violations; median +4.9d/+17%
to mean survival; COD/SOM peaks hit the 196d ceiling as eq:decay-priors intends). KEY robustness
fact: survival is structurally bounded above by days_long (pbeta<=1), so any b only slides countries
ALONG the [16,196] curve and CANNOT breach the envelope. .rda NOT rebuilt (value unchanged, comment-
only); parent regenerates. MOZ psi_star_b override confirmed absent (folded); MOZ psi_star_k=-5
retained (scale-invariant time offset). Cited spec anchor: eq:decay-priors (04-model-description.Rmd
L213-223).

Spec authority: MOSAIC-docs/04-model-description.Rmd "### Modeling environmental suitability"
(psi-star-priors block ~line 388; transform ~line 380); beta_env eq:beta2 line 193; delta
eq:delta line 205 + eq:decay-priors line 216. Engine: laser-cholera envtohuman.py:24,
environmental.py:33/150.
