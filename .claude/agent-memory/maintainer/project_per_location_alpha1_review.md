---
name: per-location-alpha1-review
description: Reviewed + APPROVED per-location alpha_1 (MOSAIC v0.51.0); the complete sibling set for a dual-mode (scalar-OR-length-nL) per-location param + verified engine parity facts
metadata:
  type: project
---

Adversarially reviewed swe's per-location `alpha_1` (MOSAIC v0.51.0, priors_default
v15.16 / config_default v4.7). **Verdict: APPROVE** (one NIT only). Build + tests
verified clean.

**The COMPLETE sibling set for a dual-mode (scalar broadcast OR length-nL per-loc)
param** — verified all handle it, use as a checklist for the next such param
(alpha_2 if it ever goes per-loc, kappa, etc.):
- `validate_sampled_config()` (sample_parameters.R): needs a NEW `scalar_or_vector`
  schema branch — the rigid global/scalar group HARD-ERRORS on length-nL (is.na(val)
  with length>1). Don't force a dual param into scalar OR vector group.
- `get_param_names.R`: add to `location_params_base`; classification needs length>1 &&
  ==nL for location, else global fallback. (Matrix path is regex-suffix-driven, generic.)
- `plot_model_parameters.R`: in BOTH `global_params` AND `location_params_base`;
  case_when checks `%in% global_params` FIRST (a bare scalar col -> Global) then
  `param_base %in% location_params_base` (alpha_1_<ISO> -> Location-Specific). OK.
- `get_location_config.R`: HARDCODED per-loc subset — must be LENGTH-AWARE
  (`length(out$x)==length(config$location_name)` guard) so a scalar is left untouched
  (subsetting scalar with [sel] -> NAs). **This was the sibling the plan MISSED**,
  caught by an engine-backed test.
- `convert_config_to_matrix.R`: forward expansion is GENERIC (length==n_locations
  branch makes `_<ISO>` cols even though alpha_1 is NOT in its location_params_base).
- `convert_matrix_to_config.R`: reverse is suffix-driven generic, BUT writes
  `config$x[idx]` only `if length(config$x) >= idx` — a SCALAR seed silently drops
  idx 2..nL. **This is why D1 (config_default ships length-nL alpha_1) is load-bearing**
  — it makes the round-trip robust. Don't ship D2-only (scalar seed + runner-expand)
  as the package default; the silent-drop trap lurks for any other caller.
- `get_location_priors.R`, `.sample_location_parameters_impl`, posterior_quantiles,
  update_priors_from_posteriors, get_param_names matrix path: all GENERIC (iterate
  names(parameters_location)), auto-capture, no per-field edit.

**Engine parity fact (verified against the INSTALLED wheel params.py:557-564, NOT the
stale src checkout):** a vector alpha_1 is coerced via
`np.asarray(params.alpha_1, dtype=np.float32)` — UNCONDITIONAL float32 cast regardless
of input type. So a float64 numpy array from a Dask worker still lands float32 =>
PARITY-SAFE => alpha_1 belongs in `_VECTOR_FIELDS`, NOT `_AS_NDARRAY_FIELDS`. The
float64-state-seeding divergence the `_AS_NDARRAY_FIELDS` header warns about only
applies to fields the engine coerces via its `as_ndarray()` helper (which returns an
incoming ndarray UNCHANGED, skipping the cast). [[project_engine_tests_ci_gap]]:
the dask schema-parity test is a serialize round-trip (no wheel) so it DOES run in CI;
the alpha_1_<ISO> assertion (test 2b) passed locally.

**NIT (not blocking):** `model/input/parameters_inventory.csv` +
`estimated_parameters.csv` still label alpha_1 scope="global". Static inventory CSVs,
not consumed by the dual-mode pipeline — stale-doc only. Owner: data-engineer/swe if
those CSVs are regenerated.

Baseline confirmed: the rebuilt .rda/.json match the .R builders exactly (priors
v15.16 global has NO alpha_1, 40 ISO @ Beta(28.4,71.6); config v4.7 alpha_1 len-40
all 0.27, alpha_2 scalar 0.5). R CMD check 1E is the pre-existing test-tree
source("../../R/...") harness issue [[project_rcmdcheck_baseline_v048]] — two changed
test files use it but the source() line predates this diff (only test_that blocks
appended). Tests pass under devtools::test() (cwd=pkg root).
