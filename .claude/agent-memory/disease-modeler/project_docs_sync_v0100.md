---
name: docs-sync-v0100
description: MOSAIC-docs brought in line with MOSAIC-pkg v0.100.0 (2026-09-30); notation choices and follow-up claims that turned out wrong
metadata:
  type: project
---

MOSAIC-docs synced to MOSAIC-pkg v0.100.0 on 2026-09-30 (commits 260981e..d43e022 on origin/main).

- New registered notation: beta_{j0}^{tot}, p_beta, omega^{mob}/gamma^{mob} (gravity exponents; bare omega/gamma collided with omega_1/2, gamma_1/2), t_0 (IC epoch), lambda_j (E/I onset rate).
- R0_env formula is UNCHANGED under the per-capita dose W/N: dose-response slope is 1/(kappa N), times S~N susceptibles gives 1/kappa. (Under old raw-W it should have carried N/kappa.) The review note "1/kappa -> 1/(kappa N)" was only half right.
- Fixed mode reports converged = FALSE + convergence_evaluated = FALSE (NOT NA as the follow-up list claimed).
- Tier search + optimizer use control$targets$best_subset_weighting (same as posterior), saturated by default.
- sigma prior Beta(4.30,13.51) is hardcoded in make_priors_default.R and was NOT refit after the Harris 2008 correction (0.184 -> 0.629); proportion_symptomatic.png will disagree with the stated prior once est_symptomatic_prop() is re-run. Open decision.
- zeta_ratio figure from est_zeta_ratio_prior() still shades the combined-channel CI and marks the combined median; needs a plotting fix before it goes into the docs.

**How to apply:** when next touching the spec, reuse these symbols; re-check the sigma prior and zeta_ratio figure before the post-rebuild docs refresh.
