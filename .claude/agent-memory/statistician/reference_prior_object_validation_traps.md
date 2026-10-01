---
name: prior-object-validation-traps
description: Traps when validating a hand-assembled priors object - sample_from_prior swallows errors to NA and sample_parameters silently substitutes the config value; sampled vectors are in config order, not member-list order; posteriors.json is 15 digits, priors.json 17
metadata:
  type: reference
---
1. **Silent fallback.** `sample_from_prior()` wraps every distribution in tryCatch and returns NA on any error: negative shape, a >= b, a malformed field. `sample_parameters()` then uses `config[[param]][i]` and emits a message only when verbose. A broken prior entry therefore produces a pinned parameter, not an error. To validate, call `sample_from_prior(n = 2000)` on every changed entry (check finite, non-constant, in support). Then compare which location fields vary across seeded `sample_parameters()` draws under base versus under the candidate. Restrict that comparison to continuous fields: E/I counts can be all zero by chance.
2. **Order.** The vectors `sample_parameters()` returns are in `get_location_config()` order, which is config_default canonical (alphabetical) order. A region's member list (ETH SOM KEN SSD UGA) is in a different order. `match(iso, members)` read the wrong column and my own validator mislabelled countries; it was caught only because a centring check fired. Label columns from `config$location_name`.
3. **Precision.** `calc_model_posterior_distributions()` writes posteriors.json with `toJSON(digits = NA)`, which keeps 15 significant digits. run_MOSAIC's priors.json uses `I(17)`. Truncation bounds copied from the prior template come back within about 1e-15 relative, not exactly equal. Compare with a relative tolerance, then snap to the base bounds.
4. **RNG stream.** Changing any prior's values shifts the RNG stream for every later draw, because rbeta rejection consumes a parameter-dependent number of uniforms. Draw-by-draw comparisons between base and candidate are not paired.

**How to apply.** Use these whenever a priors object is built outside make_priors_default.R: warm starts, staged estimation, hand patches. Negative-test the validator with deliberately broken objects before trusting it (claude/deploy_v0100/warmstart/_test_suite/negative_tests.R, 18 cases).
