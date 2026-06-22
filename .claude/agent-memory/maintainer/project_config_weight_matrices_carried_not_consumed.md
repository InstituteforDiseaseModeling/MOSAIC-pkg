---
name: config-weight-matrices-carried-not-consumed
description: config_default v4.1+ ships reported_cases_weight/reported_deaths_weight matrices that are CARRIED but not consumed by the likelihood; the Dask broadcast keep-list omits them (future wiring landmine)
metadata:
  type: project
---

config_default v4.1 (priors_default v15.12, ~MOSAIC v0.45.x) added two new n_loc x n_t matrices
`reported_cases_weight` / `reported_deaths_weight` (per-observation confidence_weight in [0,1];
direct WHO/JHU/SUPP = 1.0). They are injected POST `make_LASER_config` in
`data-raw/make_config_default.R` (into both `params_validated` JSON and `config_default` .rda),
because `make_LASER_config()` has explicit named formals and rejects unknown args.

**Why:** multi-source fit target (15->40 data countries) carries trust weights for a future
per-cell weighted likelihood. Not yet consumed by `calc_model_likelihood` (no per-cell slot).

**How to apply / verified-safe paths (confirm still true on any re-review):**
- The two matrices are EXCLUDED from `convert_config_to_matrix.R` / `convert_config_to_dataframe.R`
  `params_keep` allow-lists -> they never leak into samples.parquet. Good.
- The LASER engine ignores them: `laser-cholera .../metapop/params.py` `validate_parameters` is an
  allow-list of asserts (never rejects unknown keys) and the ndarray-conversion loop iterates a
  FIXED `arrays` list. Verified harmless to pass the full config to `paramfile`.
- `get_location_config.R` IS the only row-subset site that must keep weights row-aligned; it was
  updated via `intersect(c(...weight...), names(out))` (graceful with old weight-less configs).
  `est_epidemic_peaks`/`make_LASER_config` `reported_cases[...]` refs are NOT the config matrix.

**LANDMINE for future wiring:** `.extract_base_config()` in `run_MOSAIC_helpers.R` (Dask broadcast
keep-list) keeps `reported_cases`/`reported_deaths` but NOT the weight matrices. When someone wires
per-cell weighting into `calc_model_likelihood`, they MUST add both weight keys here or the Dask
path computes UNWEIGHTED while the local PSOCK path computes weighted -> Lesson #12 local-vs-Dask
divergence. Flag this on any PR touching the likelihood weight consumption.
