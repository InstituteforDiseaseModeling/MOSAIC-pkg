---
name: dask-local-dtype-parity-v0482
description: Dask-vs-local engine parity traps fixed in MOSAIC v0.48.2 (as_ndarray list-vs-array dtype + weight-matrix JSON-NA crash)
metadata:
  type: project
---

Two Dask/Coiled-path correctness bugs found+fixed in MOSAIC v0.48.2 (parallel
swe+maintainer audit before production scale-up). Both confirmed independently;
local PSOCK path unaffected by either.

**Why:** v0.48.0 began consuming per-cell confidence weights on the Dask worker,
exposing latent serialization/dtype bugs that silently diverged the two backends.

**How to apply:** when touching `_apply_sampled_params` / `run_laser_postca` in
`inst/python/mosaic_dask_worker.py`, or `.extract_sampled_params` /
`.extract_base_config` in `R/run_MOSAIC_helpers.R`, re-check both invariants below.

1. **as_ndarray list-vs-array dtype trap (HIGH).** The laser-cholera engine's
   `params.as_ndarray(input, dtype)` returns an incoming numpy array UNCHANGED
   (skips the dtype cast) but casts an incoming LIST to the declared dtype. IC
   counts (`{N,S,E,I,R,V1,V2}_j_initial`) are declared `uint32`; seasonality/
   transmission vectors (`tau_i, beta_j0_hum, a/b_{1,2}_j, beta_j0_env, theta_j,
   longitude, latitude`) are `float32`. The LOCAL path ships these as Python
   LISTS (reticulate r_to_py) -> engine seeds uint32 state. The Dask worker used
   to pre-convert them to float64 numpy -> engine seeded FLOAT64 state ->
   ~1-2% log-likelihood divergence from local at config_default scale even with
   identical values + identical seed. Fix: `_AS_NDARRAY_FIELDS` frozenset; keep
   those fields as lists on the worker (and convert any base_config/extra_fields
   numpy back to .tolist()). Fields coerced via `np.array` (beta_j0_tot,
   psi_star_*, prop_*_initial, mu_j_*) ALWAYS recast to float32 regardless of
   input type, so they are parity-safe and stay in `_VECTOR_FIELDS`.
   Verified bit-identical engine output (diff 0) + scorer LL agree to 2e-15.

2. **weight-matrix JSON "NA"-string crash (CRITICAL).** `.extract_sampled_params`
   did not exclude `reported_cases_weight`/`reported_deaths_weight`. They leaked
   into the per-sim JSON where `jsonlite::toJSON(digits=NA)` serializes NA cells
   as the STRING "NA"; `config.update(sampled)` on the worker then clobbered the
   clean float64 base_config arrays with lists carrying "NA" -> the v0.48.0
   weighted scorer's `np.asarray(dtype=float)` raised "could not convert string
   to float: 'NA'", failing EVERY Dask sim (+~30% per-sim JSON bloat: 1.54MB ->
   1.10MB). They are broadcast once via `.extract_base_config`; excluding from
   the per-sim set is correct.

**Verification harness pattern (reusable):** build base_config via
`r_to_py(.extract_base_config(.mosaic_inject_likelihood_settings(cfg, lik)))`,
sample via `.mosaic_sample_and_serialize`, then compare
`dict_to_propertysetex(config)` dtypes AND a same-seed `run_model` reported_cases
between the worker config (`_apply_sampled_params`) and the local config
(`.mosaic_prepare_config_for_python` -> r_to_py). Engine same-process+same-seed
is bit-reproducible (diff 0), so any nonzero diff is a config/dtype mismatch.

**Gotcha:** the read-only `laser-cholera/` checkout is OLDER than the installed
venv engine. For the alpha/as_ndarray contract, read the INSTALLED engine
`~/.virtualenvs/r-mosaic/lib/python3.12/site-packages/laser/cholera/metapop/
params.py` (0.16.0 has the dual-mode alpha branch; the checkout still has alpha
in the scalar list). Related: [[dask-worker-likelihood-v016-parity]].

**Still open (not bugs):** stale `central_method` test assertion expects "mean"
but default is "median" since v0.46.1 (maintainer domain). Engine
compute_wis_parametric_row uses np.sum not np.nansum -> WIS local/Dask divergence
over gappy data when weight_wis>0 (defaults 0; engine read-only).
