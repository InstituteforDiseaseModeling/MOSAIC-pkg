---
name: project-stage1-beta-recenter
description: beta_j0_tot per-country w=0.5 geometric-mean partial-shrinkage recenter from the Stage-1 metapop review (priors_default v15.14, recalibration-gated); + the audit that beta_j0_hum/env are derived not stored
metadata:
  type: project
---

# beta_j0_tot Stage-1 partial-shrinkage recenter (priors_default v15.14)

**The change (Fix 2 of the bundled prior fixes, statistician spec
`claude/prior_fix_spec/SPEC_prior_fixes.md`, recalibration-gated, NOT committed).**
beta_j0_tot is weakly identified (Stage-1 ESS ~110-135, second-lowest of all
params), so single-location posteriors give a reliable DIRECTION but a noisy point.
Recenter each country's meanlog as the GEOMETRIC MEAN (w=0.5) of its v15.9 prior
center and its Stage-1 single-location posterior median:
`new_center = exp(0.5*log(prior) + 0.5*log(post_median)) = sqrt(prior*post_median)`.

w=0.5 is deliberately conservative: (i) the Stage-3 warm-start inflates beta_j0_tot
x2 (single-patch estimate into a network that adds transmission), so a full recenter
to the point would fight the joint fit; (ii) ESS ~130 is genuinely uncertain;
(iii) the geometric midpoint honors direction without committing to the point.

- 18 of the 19-ISO cohort recentered. SYMMETRIC — five move UP (BDI, MWI, NGA, RWA,
  TZA) because the v15.9 v0.14.0 sweep over-corrected them down; NOT a flat cut.
- Prior WIDTH kept: sdlog stays the global 1.1748 for every country (Stage-3
  inflation guard). Only meanlog moves.
- **LBR EXCLUDED** — Stage-1 confirms it is unidentified (1724 cases over 1107
  nonzero days, max 4/day, no epidemic structure, r2_corr ~0 at every beta scale,
  1 death). Stays at the 2e-5 global default. (Open: a per-location level override
  or exclusion as a data limitation.)
- The 26 no-data countries keep 2e-5 (no Stage-1 fit, no signal).

**AUDIT RESOLVED — beta_j0_hum / beta_j0_env are NOT stored as separate priors.**
There is no beta_j0_hum/beta_j0_env location-prior block in make_priors_default.R.
They are DERIVED from beta_j0_tot and p_beta:
- at sample time: sample_parameters.R L574-575
  `beta_j0_hum[i] = p_beta[i]*beta_j0_tot[i]`,
  `beta_j0_env[i] = (1-p_beta[i])*beta_j0_tot[i]`
- at config-build time: make_config_default.R L429-430 (identical split).
So recentering beta_j0_tot propagates to hum/env consistently with NO separate edit.
Verified in config_default v4.4: ETH hum 4.6563e-06 + env 9.4537e-06 = 1.411e-05 =
beta_j0_tot (p_beta=0.33). The dm headline "0.54x" pooled hum+env; beta_j0_tot
itself moves only ~0.79 median — do not confuse the two figures.

**FLAG — COG override dropped.** The v15.9 `beta_j0_tot_v0140` vector had
COG=4.6667e-06. COG is NOT in the Stage-1 19-ISO cohort, so it reverts to the 2e-5
global default (a 4.3x increase). Consistent with "only Stage-1-fit countries
qualify" but a behavior change the spec didn't call out — re-add to the vector if
COG should retain a transmission override.

**How to apply:** the recenter vector lives in `beta_j0_tot_v0140` in
make_priors_default.R (~L742-790). If Stage-2 (where beta is better identified)
corroborates the direction, a later pass can raise w above 0.5. Keep the width;
never tighten sdlog without revisiting the Stage-3 inflation guard. Related: the
Fix 1 mu_j_baseline B1 re-anchor shipped in the same v15.14 build
([[project_eth_deaths_cfr_dwell_mismatch]]).
