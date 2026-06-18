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

Spec authority: MOSAIC-docs/04-model-description.Rmd "### Modeling environmental suitability"
(psi-star-priors block ~line 388; transform ~line 380).
