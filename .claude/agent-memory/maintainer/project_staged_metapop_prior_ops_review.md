---
name: staged-metapop-prior-ops-review
description: Adversarial review (v0.47.4) of the prior/config ops used by the staged 40-loc metapop fit; what is name-keyed-safe vs latent landmines + test gaps
metadata:
  type: project
---

Reviewed the prior/config manipulation ops the full-40 staged metapop fit relies on
(`claude/full_metapop/01..03`). All key ops are NAME-keyed (not positional), so the staged
pipeline is functionally safe; the findings are latent landmines + missing safety nets.

**Why:** before committing the metapop joint fit, user wanted confidence these behave on the
full 40-country structure (not just single-loc/toy fixtures).

**How to apply:** when these ops change, re-check the items below.

- **update_priors_from_posteriors** keys by iso NAME (`location[[iso]]`) — robust to order. On the
  real 40-loc base, folding a partial posterior preserves the other 39 locs + all globals
  byte-identical, drops isos not in base, never leaks. SAFE.
- **`.validate_updated_priors` BLIND SPOT** (`R/update_priors_from_posteriors.R:451-461`): checks
  location COUNTS per group, never NAMES. A name-swap that keeps count=40 passes validation
  silently. This is the Lesson #8/#11 class. Fix = assert `identical(sort(names(orig loc)), sort(names(upd loc)))` per group.
- **inflate_priors(2,"beta_j0_tot")** (the actual Stage-2 call): beta_j0_tot is lognormal across
  all 40 locs; inflation preserves mean exactly, var x2 exactly, zero scope leakage. Edge cases
  (beta f_max skip, truncnorm sd, uniform positivity skip, NA/sd=0/fixed) all handled. SAFE.
  NOTE: uniform on [0,x] essentially can NEVER inflate (positivity guard always trips).
- **get_location_priors vs get_location_config ORDER MISMATCH**: get_location_priors returns locs
  in REQUESTED order; get_location_config returns them in SOURCE (config) order. A positional zip
  of the two would misalign. Harmless today because sample_parameters keys priors by iso name
  (`R/sample_parameters.R:483-491`), but it is a position-vs-name landmine.
- **convert_config_to_dataframe DROPS 400 per-loc params vs convert_config_to_matrix**: the df
  allow-list (`R/convert_config_to_dataframe.R:67-124`) lists `a_1/a_2/b_1/b_2` (global scalars)
  but NOT the per-loc `a_1_j/a_2_j/b_1_j/b_2_j` nor the `prop_*_initial`. Matrix path includes them
  (1269 names vs df 869). convert_config_to_dataframe is NOT used by run_MOSAIC (diagnostic only),
  so low severity, but the two siblings are NOT interchangeable representations.
- **convert_config_to_matrix <-> convert_matrix_to_config** (production path, run_MOSAIC):
  bit-perfect round-trip on full 40-loc incl per-loc order. signature is
  `convert_matrix_to_config(param_vector, config_base, ...)` (NOT template_config). zero prod callers
  of convert_matrix_to_config though.

**Test coverage gaps (full-40 confidence):** staged tests (test-update_priors_from_posteriors,
-update_and_inflate_priors, -prior_posterior_roundtrip/-pipeline) use 0 refs to real
priors_default/config_default — all 2-loc (ETH/MOZ) toy fixtures with BOTH locs in the posterior.
Missing: (a) partial-coverage fold into 40-loc base asserting the other 38 byte-identical;
(b) validator name-drift test; (c) get_location_priors vs _config order-consistency test;
(d) convert sibling-parity test. Probes at claude/prior_config_review/maint/.
