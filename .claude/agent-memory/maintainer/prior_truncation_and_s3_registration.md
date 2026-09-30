---
name: prior-truncation-and-s3-registration
description: Adding a field to a prior family (e.g. lognormal lower/upper) must reach every prior consumer; hand-maintained NAMESPACE needs S3method() for each print.* method
metadata:
  type: feedback
---

When a review adds a new parameter to a prior distribution family (h2-priors, 2026-09-30: `lower`/`upper` truncation on lognormal for zeta_ratio), check every consumer that reads `prior$parameters`, not only the sampler:
sample_from_prior (draw), update_priors_from_posteriors (.clean_posterior_entry strips non-core fields, so bounds need explicit carry), inflate_priors, plot_model_distributions + plot_model_posteriors_detail (dlnorm/qlnorm prior curves ignore bounds), sample_parameters expected_mean diagnostic, json_io_utils validator (allows extra fields).

**Why:** the plotting and diagnostic consumers silently draw the untruncated prior, which looks right but no longer matches what is sampled.

**How to apply:** grep `dlnorm|plnorm|qlnorm|"lognormal"` in R/ for any lognormal-prior change, and do the same for other families.

NAMESPACE is maintained by hand (`exportPattern`, not roxygen). Every `print.<class>`/`summary.<class>` needs its own `S3method()` line. As of 2026-09-30, `print.mosaic_priors` (R/get_location_priors.R) was still unregistered after `print.mosaic_initial_conditions_S` got its line. Grep `^(print|summary|format)\.` in R/ against `grep S3method NAMESPACE`.
