---
name: truncated-prior-double-truncation
description: Carrying a truncation bound onto an UNTRUNCATED CI-fit of truncated posterior draws re-truncates and drifts the prior each stage (zeta_ratio lower=1: median 184->1017 in one no-information stage)
metadata:
  type: reference
---

Invariant: if a prior is truncated (e.g. zeta_ratio lognormal, lower = 1), its posterior draws are
already truncated. Fitting an untruncated family to their quantiles (fit_lognormal_from_ci: CI-only,
meanlog = mid log CI) and then re-applying the bound (update_priors_from_posteriors
`.carry_lognormal_bounds`, v0.100.0 integrate branch) truncates TWICE.

Measured (1e5 draws, LN(4.31, 4.39) lower=1, posterior == prior): stage medians 184 -> 1017 -> 1447
-> 1816 -> 2058; 2.5% quantile 1.4 -> 3.2 -> 8.3. zeta_ratio is non-identifiable so this is the
realistic case for staged / warm-start pipelines.

**Why:** staged estimation should be a fixed point when the data are uninformative.
**How to apply:** any time a bound is carried from prior to posterior, the fit must be in the
truncated family (solve meanlog/sdlog so the TRUNCATED quantiles match) — check fixed-point with a
posterior==prior round trip. Live since priors_default v17.0 (zeta_ratio lower = 1; v17.1 too).

Status: fixed on integrate/deep-review in 1f73e43dd (.fit_truncated_lognormal_ci; round-trip test in test-zeta-ratio-truncation.R).
