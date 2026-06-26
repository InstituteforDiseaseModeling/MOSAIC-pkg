---
name: run-mosaic
description: >
  Assemble or modify a MOSAIC config + priors + control and launch a run_MOSAIC()
  calibration (or a single deterministic run_LASER() sim). Covers install/environment,
  building/subsetting a config (locations, dates, psi_jt), choosing priors and the
  pin-vs-sample lever, the control object (canonical names, weights, ESS/R² targets,
  io presets), where to run (laptop vs hedgehog/dugong), and reading the output tree.
  Use for "how do I install / configure / run / deploy / build a scenario" and any
  "set up a calibration" task. Composed by the forecast-cv skill.
---

# run-mosaic — assemble a MOSAIC run and calibrate

This skill is the **official runbook for configuring and launching MOSAIC**. It documents existing,
exported functions — it does not introduce new workflow. Produce concrete, runnable snippets with a
one-line explanation of each knob. **Reference, don't transcribe:** numeric defaults, prior centers,
and versions drift — cite the source of truth (`?fn`, the version-note in `data-raw/`, the spec
`.Rmd`) rather than hard-coding a value that will rot.

## Documentation tiers (consult in this order; cite which repo)
1. **Roxygen man pages (most version-stable):** `?run_MOSAIC`, `?run_LASER`, `?make_LASER_config`,
   `?sample_parameters`, `?mosaic_control_defaults`, `?calc_model_ensemble`. Primary source for
   args + return contracts.
2. **MOSAIC-pkg vignettes + examples:** `vignettes/Installation.Rmd`, `Running-MOSAIC.Rmd`,
   `Running-LASER.Rmd`, `Deployment.Rmd`; `inst/examples/simulate_outbreak_settings.R` (5 regimes).
3. **MOSAIC-docs sibling repo (canonical model/param spec — read the `.Rmd`, not rendered `.md`):**
   `04-model-description.Rmd` ("Table of model parameters" = symbol→meaning for every parameter),
   `05-model-calibration.Rmd` (BFRS methodology), `06-scenarios.Rmd` (scenario construction).

## 0. Install & environment (verify first)
- `MOSAIC::check_dependencies()`; if broken: `MOSAIC::remove_python_env(force = TRUE)` →
  `MOSAIC::install_dependencies(force = TRUE)` → restart R.
- Python env at `~/.virtualenvs/r-mosaic` (laser-cholera + laser-core + numpy/h5py/pyarrow). System
  libs: GDAL/PROJ/GEOS. Set the root once: `MOSAIC::set_root_directory("~/MOSAIC")`.

## 1. Assemble the config
- Start from `MOSAIC::config_default` (40-country SSA, ψ baked in) or build/subset with
  `get_location_config()` for a single country or a coupled-metapopulation **vector** of ISO3 codes.
- The config window (`date_start`/`date_stop`) sets the simulation span; `date_start` is the anchor.
- ψ enters as the `psi_jt` matrix (`locations × dates`). `make_LASER_config()` validates the full
  config (incl. `ncol(psi_jt) == length(t)`). To use a freshly fit ψ, see the **`est-suitability`**
  skill (it writes `model/input/pred_psi_suitability_day.csv`; the bake path is
  `data-raw/make_config_default.R`).

## 2. Priors — and the pin-vs-sample lever
`MOSAIC::priors_default` carries per-country priors. **Always check the live version + inline
version-note in `data-raw/make_priors_default.R` before trusting a center — do not transcribe
numbers here.** Key levers (semantics, not values):
- **Deaths / CFR level → `CFR_target`, NOT `mu_j_baseline`.** `mu_j_baseline` is *derived* at sample
  time from `CFR_target` (see `?sample_parameters` / the version-note); pinning `mu_j_baseline`
  directly is silently overwritten when `sample_mu_j_baseline=TRUE` (the default) — set it FALSE to
  keep the config value. Toggle the deaths chain with `sample_CFR_target` / `sample_mu_j_baseline`. The
  v0.13 `rho_deaths` factor is already baked in — do not re-apply it.
- **`alpha_1`** is a per-location sampled prior (shared informative Beta) that emulates hierarchical
  shrinkage to starve the `alpha_1 ↔ beta_j0_tot` degeneracy — sampled by default; check the
  version-note for the current center.
- **ψ\* transform (`psi_star_a/b/z/k`)** is sampled and re-applies to `psi_jt` per simulation:
  `a` = logit-scale **gain** (a→0 flattens ψ* to a constant ⇒ ψ's *shape* is discarded), `b` = level
  offset (feeds the δ reservoir-survival channel; coupled to the `response_var` normalization —
  see `est-suitability` B2a), `z` = causal-EWMA weight, `k` = day offset. Toggle via
  `sample_psi_star_*`. **To force the model to use a supplied ψ, pin ψ\* near identity rather than
  letting calibration attenuate it.**
- **Pin a parameter** by setting its `sample_*` flag FALSE (it stays at the config value); leave TRUE
  to sample from the prior.

## 3. Control object
`control = MOSAIC::mosaic_control_defaults()` then override. **Use the canonical key names** —
`*_adaptive` / `*_total` / `ESS_method` (e.g. `target_r2_adaptive`, `n_simulations`,
`max_simulations_total`). Legacy names (`batch_size`, `target_r2`, `max_simulations`, `ess_method`,
…) are **silently dropped** if you also rely on defaults (see CLAUDE.md **Lesson #13**) — there is no
unknown-key validator, so a typo'd/legacy key reverts to default with no warning. Common levers:
`weight_cases` / `weight_deaths`, ESS thresholds, `n_iter_ensemble`, `clean_output`, and the `io`
preset (`default` / `debug` / `fast` / `archive`; `?mosaic_io_presets`). Multi-location-only samplers:
`sample_tau_i`, `sample_mobility_*`. `central_method` (`"median"` default vs `"mean"`) sets the
ensemble central tendency — see `?calc_model_ensemble`; a ~2× deaths bias under `"mean"` is by
design (unmasks implied CFR), not a regression.

## 4. Launch
```r
res <- MOSAIC::run_MOSAIC(config = cfg, priors = MOSAIC::priors_default,
                          control = ctrl, dask_spec = NULL)   # NULL = local PSOCK
```
- **Single deterministic sim** (scenario exploration / teaching): `run_LASER()` with a fixed config +
  seed.
- **Output fields:** read `model$results$reported_cases` / `reported_deaths` — NOT raw `disease_*`
  (post-v0.13 convention).
- **Thread safety (required for any custom parallel code):** set
  `OMP_NUM_THREADS=MKL_NUM_THREADS=OPENBLAS_NUM_THREADS=NUMEXPR_NUM_THREADS=TBB_NUM_THREADS=NUMBA_NUM_THREADS=1`
  and `MOSAIC:::.mosaic_set_blas_threads(1L)` (built into `run_MOSAIC()`).

## 5. Where to run
A 40-country coupled calibration is a **hedgehog/dugong** job (~2 GB/worker), not a laptop job — use
the **`hedgehog-run`** / **`dugong-run`** skills for VM mechanics (R wrapper, surviving disconnect,
pulling results). On dugong, a run that invokes laser **requires** the `r-mosaic-Rscript` wrapper.

## 6. Output tree
```
dir_output/
├── 1_inputs/        # config.json (RAW psi_jt input), priors.json, control.json, environment.json
├── 2_calibration/   # samples.parquet, posterior/, diagnostics/, state/, ensemble_*.rds,
│                    #   best_model/config_medoid.json
└── 3_results/       # summary.json, predictions/, figures/
```
**There is no `config_best.json`** — `run_MOSAIC()` intentionally produces no best-likelihood model.
The post-calibration artifacts are `2_calibration/best_model/config_medoid.json` + the ensemble RDS
files (`ensemble_candidate.rds`, `ensemble_optimized.rds`, `medoid_ensemble.rds`).

## Hand-offs
- Run *came out wrong* / bias / convergence interpretation → **`diagnose-fit`** skill /
  `calibration-doctor`.
- Systematic forecast cross-validation → **`forecast-cv`** skill.
- Fit / predict ψ for a time frame, or refresh climate inputs → **`est-suitability`** skill.
- A genuine source bug → route to the owning dev agent (`swe` / `statistician` / `disease-modeler`
  / `ml-scientist`). This skill is operator-facing; it does not edit package source.
