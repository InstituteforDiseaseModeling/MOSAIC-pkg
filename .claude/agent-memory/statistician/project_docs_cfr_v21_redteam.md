---
name: docs-cfr-v21-redteam
description: 2026-09-29 red-team of MOSAIC-docs CFR v2.1 + mobility rewrite vs pkg v0.99.9 — verified facts, doc/code gaps found (medoid loc-1 only, mobility data-raw not reproducing shipped defaults)
metadata:
  type: project
---
Docs pushed 3e152f1..19eddc2 (MOSAIC-docs main). Verified against v0.99.9 code:
- Integrated deaths Laplace marginal == brute-force 2-D integral to 0.003 nats (check: week_offset MUST be fixed, else auto-detected cadence changes the week partition and the log D! terms, ~4 nats apparent gap).
- Poisson CFR score is only APPROXIMATELY total-preserving (prior shrinkage + bg + logit p(1-p)); 38.5 vs 39 in a toy.
- rho_deaths cancels from reported deaths IN DISTRIBUTION (Binom thinning), but not inert: sets fatal onsets withheld from I1 and the p_fatal<1 bound.
- Legacy markers = mu_j_baseline, mu_j_epidemic_factor, CFR_target, mu_j (NOT delta_reporting_deaths).
- Default ESS_method = perplexity, ESS_marginal_method = kde (calc_model_ess_parameter roxygen line 4 still says binned default).

**Open code issues (reported, not fixed):** medoid distance uses location 1 only (run_MOSAIC.R ~2798); data-raw/make_config_default.R + make_priors_default.R read mobility_gravity_params.csv (omega 0.616, gamma 1.362) and air-fit Beta tau priors, yet shipped config/priors carry blend 0.627/1.900 and lognormal overland tau — a rebuild would silently revert mobility. Engine applies p_fatal[tick] to onsets recorded at tick+1 while likelihood/posthoc use mu at the onset column (1-day offset, immaterial).

**How to apply:** when a doc symbol is added, check collisions with the style-guide registry (d_jt background mortality, W reservoir, K_j now used for Laplace dim).
