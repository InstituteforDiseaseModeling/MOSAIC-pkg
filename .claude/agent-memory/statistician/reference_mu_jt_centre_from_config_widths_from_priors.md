---
name: mu-jt-centre-from-config-widths-from-priors
description: The integrated deaths likelihood takes the CFR centre from config$mu_jt and only the widths (sd_year, sd_product, logit_se over observed years) from priors$mu_jt; logit_mean in priors is never read, so folding a CFR posterior there is inert
metadata:
  type: reference
---
**Invariant (v0.96+, checked at v0.100.1).** `.mosaic_resolve_deaths_integration()` (R/calc_log_likelihood_deaths_integrated.R) works as follows:
- The centre is `base_logit_full` from `.mosaic_config_mu_jt(config)`, i.e. `config$mu_jt`.
- From `priors$mu_jt` it reads only `sd_year` and `sd_product`, plus each location's `logit_se`, which it averages as sqrt(mean(se^2)) over the years that location observes deaths.
- `priors$mu_jt$location$X$logit_mean` has no consumer. Only `make_mu_jt.R` builds it.

**Why it matters.** Rewriting `logit_mean` from a run's `cfr_posterior.csv` changes nothing. A fold that does act must move `config$mu_jt` or shrink the widths, and either one re-uses the deaths data.

There is a further reason not to fold. The CFR is integrated out per path by a Laplace step (one location offset a_j plus one deviation per year), so it is never sampled, and a warm start buys no proposal efficiency for it. A national CFR posterior is also conditional on that fit's case trajectory. In 300-sim smoke runs, the CFR location-offset z-score tracked the case bias: UGA bias 33x gave z = -4.4, KEN 4.7x gave -3.3, COD 1.3x gave +0.04.

**How to apply.** Keep the base `mu_jt` block for staged or warm-start priors. Report national CFR versus prior as a diagnostic; that is `warmstart_cfr_check.csv` in [[warmstart-v0100-design]].
