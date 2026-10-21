---
name: run-mosaic
description: >
  Assemble or modify a MOSAIC config + priors + control and launch a run_MOSAIC()
  calibration (or a single deterministic run_simulation() sim). Covers install/environment,
  building/subsetting a config (locations, dates, psi_jt), choosing priors and the
  pin-vs-sample lever, the control object (canonical names, weights, the 1.4.0 convergence status,
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
1. **Roxygen man pages (most version-stable):** `?run_MOSAIC`, `?run_simulation`, `?make_simulation_config`,
   `?sample_parameters`, `?mosaic_control_defaults`, `?calc_model_ensemble`. Primary source for
   args + return contracts.
2. **MOSAIC-pkg vignettes + examples:** `vignettes/Installation.Rmd`, `Running-MOSAIC.Rmd`,
   `Running-simulations.Rmd`, `Deployment.Rmd`; `inst/examples/simulate_outbreak_settings.R` (5 regimes).
3. **MOSAIC-docs sibling repo (canonical model/param spec — read the `.Rmd`, not rendered `.md`):**
   `04-model-description.Rmd` ("Table of model parameters" = symbol→meaning for every parameter),
   `05-model-calibration.Rmd` (BFRS methodology), `06-scenarios.Rmd` (scenario construction).

## 0. Install & environment (verify first)
- `MOSAIC::check_dependencies()`; if broken: `MOSAIC::remove_python_env(force = TRUE)` →
  `MOSAIC::install_dependencies(force = TRUE)` → restart R.
- Python env at `~/.virtualenvs/r-mosaic` (numpy + tensorflow). Needed only by `est_suitability()`;
  simulation and calibration are pure R and run without it. System
  libs: GDAL/PROJ/GEOS. Set the root once: `MOSAIC::set_root_directory("~/MOSAIC")`.

## 1. Assemble the config
- Start from `MOSAIC::config_default` (40-country SSA, ψ baked in) or build/subset with
  `get_location_config()` for a single country or a coupled-metapopulation **vector** of ISO3 codes.
- The config window (`date_start`/`date_stop`) sets the simulation span; `date_start` is the anchor.
- ψ enters as the `psi_jt` matrix (`locations × dates`). `make_simulation_config()` validates the full
  config (incl. `ncol(psi_jt) == length(t)`). To use a freshly fit ψ, see the **`est-suitability`**
  skill (it writes `model/input/pred_psi_suitability_day.csv`; the bake path is
  `data-raw/make_config_default.R`).

### Back-history / alternate-ψ config rebuild
To rebuild `config_default` at another start date (or with an alternate ψ), set the env var for
every step and rebuild both data objects twice (the default start is 2018-01-01 since v0.103.0;
the earliest is the ψ prediction start):
```bash
export MOSAIC_BUILD_DATE_START=YYYY-01-01   # priors ic_t0 = date_start (initial conditions at the start)
Rscript data-raw/make_priors_default.R      # 1. rebuild priors
Rscript -e 'devtools::install(".")'         # 2. install (config build reads the new priors)
Rscript data-raw/make_config_default.R      # 3. rebuild config
Rscript -e 'devtools::install(".")'         # 4. install
# 5-8. repeat 1-4 once: est_initial_V1_V2() divides by the INSTALLED config's populations, so
#      after a window move the first priors pass is off (V1/V2 x the population ratio, ~1.13
#      between 2018 and 2023); the second pass is the fixed point (a third reproduces it).
```
To bake an **alternate ψ** (e.g. NMME), stage the ψ DAY csv at `model/input/pred_psi_suitability_day.csv`
**before** step 3 — the config build reads it for `date_stop` + `psi_jt`. Do NOT hand-inject `psi_jt`
into an existing config (→ all -Inf likelihoods); rebuild. See the **`est-suitability`** skill and the
psi-refit memory.

## 2. Priors — and the pin-vs-sample lever
`MOSAIC::priors_default` carries per-country priors. **Always check the live version + inline
version-note in `data-raw/make_priors_default.R` before trusting a center — do not transcribe
numbers here.** Key levers (semantics, not values):
- **Deaths / CFR level → `config$mu_jt` (MOSAIC >= v0.96.0), not a sampled prior.** The reported CFR
  is a location x day matrix built by `make_mu_jt()` from `est_CFR_hierarchical()`; calibration
  integrates it out per simulated path around that centre (priors `mu_jt` block: `sd_year`,
  `sd_product`), and the calibrated value is `3_results/posterior/cfr_posterior.csv` (also written into
  `config_medoid.json`). `mu_j_baseline` / `CFR_target` / `mu_j_epidemic_factor` /
  `delta_reporting_deaths` and their `sample_*` flags are retired (warn and ignored). `rho_deaths`
  cancels from reported deaths (it sets only true deaths).
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
`*_adaptive` / `*_total` / `ESS_method` (e.g. `batch_size_adaptive`, `n_simulations`,
`max_simulations_total`). The renamed legacy names (`batch_size`, `min_batches`, `max_predictive_batch`,
`max_simulations`, `ess_method`, and since 1.4.0 `targets$best_subset_weighting` ->
`targets$posterior_weighting`) are honoured with a deprecation warning, or ignored with a warning if the
canonical key is also set (v0.37.1; CLAUDE.md **Lesson #13**). **Retired in 1.4.0** (warned and ignored):
`targets$ESS_param`, `ESS_param_prop`, `batch_ratio_max`, `calibration$max_batches_adaptive`,
`max_batches_predictive`, `target_r2_adaptive` (and their old names `max_batches`, `target_r2`),
`weights$floor`, `io$verbose_weights`; `A_best` / `CVw_best` / `min_best_subset` / `max_best_subset`
apply only under `posterior_weighting = "saturated"`/`"tempered"`.

**1.4.0 posterior, ensemble and convergence keys** (`?mosaic_control_defaults`):
`targets$posterior_weighting` (`"bestfit"` default: the best-fitting draws holding
`best_subset_ratio` = 0.2 of the posterior ESS; `"coarsened"` = 1.3.0 resample; `"saturated"` = pre-1.3.0),
`coarsening_alpha` (10), `ESS_best` (200, the ensemble ESS for GREEN), `min_ensemble_members` /
`max_ensemble_members` (30 / 1000; lower the max for continental runs), `status_thresholds`;
`calibration$min_draws` (5000) and `min_batches_adaptive` (5) before any stop, `max_simulations_total`
(150000, the budget cap), `batch_size_adaptive`, `max_batch_predictive` (cap on projected batches). The retired `likelihood$nb_k_min_cases`/`nb_k_min_deaths` warn and are ignored
(dispersion is estimated per location; `likelihood$nb_k_cases` *replaces* the estimate), and
`io$format = "csv"` warns and is coerced to parquet. Any *other* unknown or typo'd key is still
**silently ignored** — there is no general unknown-key validator, so check spelling. Common levers:
`weight_cases` / `weight_deaths`, `ESS_best` / `max_ensemble_members`, `n_iter_ensemble`, `clean_output`, and the `io`
preset (`default` / `debug` / `fast` / `archive`; `?mosaic_io_presets`). Multi-location-only samplers:
`sample_tau_i`, `sample_mobility_*`. `central_method` (default `c(cases = "median", deaths = "mean")`
since v0.101.0; `"mean"` reproduces v0.98.0-v0.100.x, `"median"` v0.46.1-v0.97.x) sets the ensemble
central line, and with it `r2_*_ensemble`/`bias_ratio_*_ensemble` — see `?calc_model_ensemble`.

**FIXED vs AUTO mode (matters for resumability and for what you can measure):**
- `n_simulations = <integer>` ⇒ **FIXED**: runs exactly that many simulations in a single batch.
  The convergence status is **assessed at the end, not used to stop**: `summary.json` has
  `convergence_status` (green/yellow/red) with the `fixed_budget` flag and `stopping_rule_met = null`;
  the legacy alias `converged` stays FALSE (deprecated). The run ends with
  `[RUN_SUMMARY] status=completed_fixed mode=fixed convergence_status=<colour> stopping_rule_met=NA`
  (`completed_fixed_partial` if the ensemble metrics are missing) — EXPECTED, not an error.
  Use it when you want a predictable runtime or a controlled comparison — it is the only mode in
  which two runs do the same amount of work. Note the whole budget dispatches as one batch, so
  mid-run resume is coarse: shards land per simulation, but the adaptive checkpointing that AUTO
  does per batch is absent.
- `n_simulations = NULL` ⇒ **AUTO** (adaptive): each batch is dispatch→gather→write→checkpoint, so
  per-batch resume works. After every batch the convergence status is computed on all draws so far;
  the run **stops at GREEN** once `min_batches_adaptive` and `min_draws` are met, else at
  `max_simulations_total` (flag `budget_cap_reached`; YELLOW never stops a run). Batches are sized from
  the projection N x 1000 / ESS_all while k-hat < 0.5 and the ESS growth slope >= 0.8, otherwise grow by
  a quarter of the draws so far. `run_state.json` carries the per-check `convergence_history`. Prefer
  AUTO for production fits.
- **Reading the status** (`3_results/summary.json`, schema 2): `convergence_status` + `convergence_reasons`
  (one per non-green check, with the projected draws) + `convergence_flags`; the four checks are ensemble ESS
  (`ess_best` vs 200), Monte Carlo error (`mce_p90` vs 10%), Pareto k-hat (`khat_tempered` vs 0.5) and data
  informativeness (`flat_likelihood`). It measures **simulation adequacy only** — fit is `r2_*_ensemble` /
  `bias_ratio_*`. Every number is in `3_results/calibration_diagnostics.json`; with `plots = TRUE` it is drawn
  as `3_results/figures/diagnostics/calibration_report.pdf`. Deprecated aliases (one release):
  `converged`, `convergence_evaluated`, `posthoc_criteria_met` (= green), `weight_health_status`,
  `ess_coarsened`, `n_best_subset`, `ess_min`/`ess_median`, `ess_target`.
- Pinning FIXED pins only the SIMULATIONS. Post-calibration still runs `n_iter_ensemble` × members
  and `n_iter_best` medoid re-simulations — hundreds to thousands of extra engine runs. For a
  like-for-like timing comparison also pin `targets$min_ensemble_members == targets$max_ensemble_members`,
  `predictions$n_iter_ensemble` and `predictions$n_iter_best`.
- Validation rule: `batch_size_adaptive` must be strictly **<** `max_simulations_total`.
- Resume: `run_MOSAIC(resume = TRUE)` requires `clean_output = FALSE`. A **non-resume** run into an
  existing `dir_output` moves leftover `sim_*.parquet` shards to `2_calibration/samples_stale_<time>/`
  (with a warning; never pooled) and deletes the post-calibration data artifacts before rebuilding
  them (ensemble RDS files, `config_medoid.json`, prediction/trajectory CSVs, the fit/optimizer/
  HSIC/CFR tables, and the post-hoc `reproductive_numbers.{csv,rds}`). Figures and other files
  are only replaced when re-written, so use `clean_output = TRUE` for a guaranteed-empty tree.

## 4. Launch
```r
res <- MOSAIC::run_MOSAIC(config = cfg, priors = MOSAIC::priors_default,
                          control = ctrl)
```
- **Single deterministic sim** (scenario exploration / teaching): `run_simulation()` with a fixed config +
  seed.
- **Output fields:** read `model$results$reported_cases` / `reported_deaths` — NOT raw `disease_*`
  (post-v0.13 convention).
- **Thread safety (required for any custom parallel code):** set
  `OMP_NUM_THREADS=MKL_NUM_THREADS=OPENBLAS_NUM_THREADS=NUMEXPR_NUM_THREADS=TBB_NUM_THREADS=NUMBA_NUM_THREADS=1`
  and `MOSAIC:::.mosaic_set_blas_threads(1L)` (built into `run_MOSAIC()`).

## 5. Where to run
A 40-country coupled calibration is a **hedgehog/dugong** job (~1.0 GB/worker as of v0.73.0; the old ~2 GB figure was the Python engine), not a laptop job — use
the **`hedgehog-run`** / **`dugong-run`** skills for VM mechanics (R wrapper, surviving disconnect,
pulling results). The engine is pure R as of v0.68.0, so a calibration no longer touches Python at all; dugong's `r-mosaic-Rscript` wrapper is now only needed for the TensorFlow psi path. Unverified on the VM — test before relying on it.

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
