---
name: per-location-alpha1-wiring
description: alpha_1 made per-location (length-nL) end-to-end in MOSAIC v0.51.0; the non-obvious sibling that the plan missed was get_location_config
metadata:
  type: project
---

alpha_1 (within-metapop mixing exponent) relocated from a global scalar to a
**per-location** quantity (length-nL vector) end-to-end. Implemented v0.51.0
(priors_default v15.16 shared Beta(28.4,71.6) per ISO; config_default v4.7 stores
alpha_1 = rep(0.27, nL)). alpha_2 stays a GLOBAL SCALAR by design.

**Why:** drive per-location mixing while a tight shared prior emulates
hierarchical shrinkage (the per-ISO sampler can't express a true hierarchy) and
starves the alpha_1<->beta_j0_tot degeneracy. Engine (laser-cholera 0.16.0
installed wheel) is already dual-mode: scalar broadcast OR length-(num_nodes,)
applied elementwise in the FOI.

**How to apply / gotchas for future dual-mode-param work:**
- The **installed wheel** is what reticulate runs, NOT the local `laser-cholera/src`
  checkout. The src tree here is STALE/divergent (treats alpha_1 as a plain scalar
  with an assert that breaks on arrays); the installed wheel
  (`~/.virtualenvs/r-mosaic/.../laser/cholera/metapop/params.py`) has the real
  dual-mode branch. Always verify the engine contract against the installed wheel.
- Dask parity: dual-mode np.asarray-coerced fields (alpha_1) go in
  `_VECTOR_FIELDS`, NOT `_AS_NDARRAY_FIELDS` — the engine recasts to float32 via
  np.asarray regardless of input type, so they're parity-safe (unlike as_ndarray
  fields which skip the cast on incoming arrays -> float64 state-seeding divergence).
- **The sibling the dev plan MISSED (Lesson #7):** `R/get_location_config.R` has a
  HARDCODED per-location field list (`location_params <- c(...)`) that position-
  subsets every per-location vector with `[sel]`. A new per-location param must be
  added there OR it ships full-length into a subset config and the engine asserts
  `alpha_1 array shape (40,) does not match (num_nodes,) = (2,)`. Caught only by the
  engine-backed `test-dask_worker_score_window_parity.R`. Fix must be LENGTH-AWARE:
  subset only when `length(out$alpha_1) == length(config$location_name)` so a scalar
  alpha_1 (back-compat) is left untouched (subsetting a scalar with [sel] -> NAs).
- The matrix<->config round-trip (convert_config_to_matrix / _to_dataframe /
  convert_matrix_to_config) and get_param_names are LENGTH-driven and generic — they
  auto-handle length-nL -> alpha_1_<ISO> with no per-field edit (only needed alpha_1
  in `location_params_base` for the config-object classification path).
- validate_sampled_config needed a new `scalar_or_vector` (dual) schema branch — the
  rigid `global`/scalar group HARD-ERRORS on a length-nL value (`is.na(val)` with
  length>1). Don't force a dual-mode param into either the scalar or vector group.
- R CMD check baseline for this pkg has a PRE-EXISTING 1-ERROR: tests that
  `source("../../R/...")` (e.g. test-plot_model_parameters.R, test-sample_from_prior.R)
  fail in the .Rcheck tree ("cannot open the connection"); they only run under
  devtools::test() (cwd = pkg root). Not a regression.
