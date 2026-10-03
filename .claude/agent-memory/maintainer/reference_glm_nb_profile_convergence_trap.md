---
name: glm-nb-profile-convergence-trap
description: Profiling NB theta with glm(family=negative.binomial(theta)) silently fails below a theta threshold on bursty series; an optimizer that maps failure to -1e10 reports the convergence boundary as the MLE (RWA 2.63 vs true 1.37, v0.101.0)
metadata:
  type: reference
---

R's glm IRLS for `MASS::negative.binomial(theta)` errors with "NA/NaN/Inf in 'x'" when the spline mean
runs to ~0 in long zero runs (fitted Poisson min 2e-16). On config_default v6.1 RWA (df-7 spline, 175
observed weeks), it errors for EVERY theta <= 2.63 from the Poisson start and even from a warm start at
the true optimum; it first converges at 2.7. `MASS::glm.nb` fails the same way (that is why
`.nb_disp_fit_one()` drops into its `theta.ml` fallback).

**The trap:** a profile function `pl(theta)` that returns `-1e10` / `NA` on non-convergence, fed to
`optimize()`, lands on the lower edge of the region where glm converges and looks like an interior
maximum. A v0.101.0 finding claimed RWA's joint NB MLE was 2.63 (corrected 2.45, "2.2x the fallback")
this way. Direct optimisation of the weighted NB log-likelihood (BFGS -> nlminb, analytic gradient,
continuation over theta; full joint over (beta, log theta) from 5 starts) gives theta 1.36-1.37,
log-lik 3.97 nats HIGHER, so the claim was refuted. The fallback (1.17, corrected 1.09) was only ~14% low.

**How to apply:** whenever a review cites a "joint"/"profile" NB MLE, check glm convergence at the
reported optimum's NEIGHBOURS (below and above). Any achieved likelihood value is a valid lower bound on
the profile, so one direct-optimisation point that beats the claimed maximum refutes it outright. Do not
recommend "profile theta with glm(negative.binomial)" as a fix for glm.nb failures: it inherits the same
IRLS failure and biases theta UP. Scripts: claude/v0101_final_review/verify-priors-trend/rwa_joint_careful.R.
Related: [[nb-dispersion-review-v092]], [[nb-dispersion-uga-floor-v0101]].
