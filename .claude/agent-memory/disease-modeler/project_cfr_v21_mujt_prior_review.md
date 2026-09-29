---
name: cfr-v21-mujt-prior-review
description: 2026-09-28 review of the uncommitted CFR v2.1 mu_jt prior (est_CFR_hierarchical rewrite, worktree cfr-v21): GAM faithful + reproducible, but config/priors builders unfinished and mu_jt = logit MEDIAN (excludes e_jy)
metadata:
  type: project
---

The est_CFR_hierarchical() rewrite (bam, all WHO-annual years 1970-2026, s(obs) RE, predictive sd = sqrt(se^2+sigma^2), +tau^2 for unseen) is faithful to spec §8.4. It reproduces the prototype CSV to 1e-3 logit and re-runs bit-identically: tau 0.616, sigma 0.696, 7.2 min with validate=TRUE. Rolling-origin 95% coverage is 0.83-0.86.

Open defects found at review:
- make_config_default.R is half-edited: it still references the deleted mu_j_baseline, mu_j_epidemic_factor and CFR_target, and never passes mu_jt.
- make_priors_default.R is unmodified. It reads param_mu_disease_mortality.csv filtering on parameter_name=="mean", which now matches both the point and the logitnormal rows, so every country silently falls back to 2%. There is also no priors$mu_jt (v16.0) entry, so D7 uses a default logit_se of 0.3 everywhere.

**Why:** these defects silently break a rebuild, so check them before any v2.1 rebuild.

**How to apply:**
- mu_jt centres are plogis(trend), i.e. the year-level MEDIAN. That is about exp(sigma^2/2), roughly 1.25x, below the expected annual CFR, so in-engine deaths run low wherever the year deviations are not integrated.
- On endemic ticks the realized reported CFR is mu_jt * chi_endemic/chi_epidemic (about 0.68x).
- Countries whose WHO data ends early (SOM 2022, BFA/LBR 2022) get fs-extrapolated drift through 2026 that is not flagged is_forecast.

Related: [[reference_cfr_mu_j0_identity]], [[project_mu_j_epidemic_factor_prior]].
