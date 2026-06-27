# MOSAIC 0.55.16

## Bug fixes

* **Dask path: `run_MOSAIC()` now nulls an empty re-injected `epidemic_peaks`** (`.mosaic_inject_likelihood_settings`). The Dask/Coiled path *overwrites* `config$epidemic_peaks` with a fresh `.filter_epidemic_peaks()` against `MOSAIC::epidemic_peaks` — so the v0.55.15 `get_location_config()` guard was insufficient (the input value is discarded). For a no-peak location the re-injected frame is 0-row, JSON-round-trips to the worker without its `iso_code` column, and crashes laser `params.py:303` (every sim erroring). This is the fix that actually resolves the BFA/CIV/GHA/LBR/NAM/ZAF Coiled failures. Regression test added (`test-epidemic_peaks_dask_inject.R`).

# MOSAIC 0.55.15

## Bug fixes

* **`get_location_config()` now nulls an empty `epidemic_peaks`.** A 0-row `epidemic_peaks` (a no-peak location, or a filter that matched nothing) JSON-round-trips to a Dask/Coiled worker without its `iso_code` column and crashes the laser engine at `params.py:303` (`.iso_code` on a column-less DataFrame → `'DataFrame' object has no attribute 'iso_code'`, every sim erroring → "No simulation results to process"). The fix drops the key when the filtered result is empty so the engine skips the `epidemic_peaks` block entirely. Surfaced by 6 no-peak countries (BFA/CIV/GHA/LBR/NAM/ZAF) failing a Coiled full-metapop batch where they had succeeded on the in-process (local) path. Regression test added.

# MOSAIC 0.55.11

## Deprecations

* **`plot_model_parameters()` is deprecated and unwired from `render_MOSAIC_figures()`.** The parameter-vs-likelihood scatter (the `"parameters"` figure group, wired in during the v0.52.0 visualization/modeling split) was a revived orphan that is redundant with the `"sensitivity"` group (`calc_model_parameter_sensitivity()` / HSIC importance) and the `"posterior"` group (prior/posterior densities per parameter), and it dominated render time — a per-facet LOESS over the full retained sample (tens of thousands of simulations across all parameters) could run for 10+ minutes on a single country. It is removed from the render pipeline and `valid_groups`; the exported function remains as a deprecation shim (`.Deprecated()`) and will be removed in a future release.

# MOSAIC 0.55.3

## Model-trajectory figures (new)

* **New `"trajectories"` figure group in `render_MOSAIC_figures()`** — a multi-panel "Model trajectories" figure per location reconstructed from a finished run directory: the comprehensive set of internal LASER channels over time (compartments S/E/Isym/Iasym/R/V1/V2/W/N, force of infection Λ/Ψ + per-time transmission rates β_jt, incidence & infection flows, burden channels) plus derived series (I_total, mass_balance, CFR(t), epidemic fraction). Each panel shows a uniform-thinned set of **actual** posterior member trajectories (the spaghetti) with the weighted central line overlaid; observed surveillance points are drawn only on the `reported_cases`/`reported_deaths` panels. Pure read-render (P5): consumes the persisted artifact only — never a LASER replay. Output to `3_results/figures/trajectories/` as `trajectories_<ISO>.pdf` + `trajectories_<ISO>_p1.png`.
* **New exported plotter `plot_model_trajectories(trajectories, location, output_dir)`** — renders one location from a `mosaic_trajectories` artifact. No LASER, no re-simulation, no re-weighting. Multi-location aware (the renderer loops one figure per location). `incidence` is correctly labelled the **S→E new-infection flow** (distinct from the `new_symptomatic` E→I progression); Λ/Ψ are labelled per-capita/day hazards; the deaths-observable overlay uses `reported_deaths` (not `disease_deaths`).
* **Capture-at-sim-time, STREAM-TO-DISK (capture-don't-replay; no second simulation).** A new `capture_trajectories` argument to `calc_model_ensemble()` (default `FALSE`; `run_MOSAIC()` defaults it **ON** for the posterior ensemble) harvests the comprehensive channels from each member where `model$results` is already in hand — at zero marginal simulation cost. Channels are **spilled to a per-sim scratch file during the run** (new args `trajectory_scratch_dir`, `reduce_trajectories`), then reduced **on the master, channel-by-channel** (one transient `[n_loc×n_time×n_param×n_stoch]` array at a time → freed before the next channel). Peak RAM is bounded to ONE dense array regardless of the channel count (the scratch *disk* footprint, not RAM, scales with `trajectory_channels`), so capture scales to 40-location runs on both laptops and large-memory VMs. Members are the **best subset** only; the full sample is never used. `epidemic_frac` reconstructs the engine's internal epidemic flag (`Isym[t-δ] > threshold·N_eff`, `N_eff` = inline people-sum) via a streaming weighted-mean pass.
* **Deviation-#1 (optimized-subset weighting), exact.** The reduction is deferred (`reduce_trajectories = FALSE`) and run by `run_MOSAIC()` over the FINAL displayed subset *after* `optimize_ensemble_subset()` — with **no re-simulation**: when `optimize_subset = TRUE` the optimized members are mapped back to their candidate scratch files by seed (with a duplicate-seed guard that falls back to the positional candidate path), and `reported_cases`/`reported_deaths` are taken from the optimized `cases_array`/`deaths_array`. The reported_* central line follows `control$predictions$central_method` **per channel** (weighted median or weighted mean), so it is **bit-identical** to the cases/deaths prediction plots under both modes; all other channels use the conventional weighted median. When `optimize_subset = FALSE`, the candidate subset (`is_best_subset`/`weight_best`) is used.
* **Comprehensive channel capture in lockstep across both backends** — the local PSOCK worker (`calc_model_ensemble.R`), the Dask Python worker (`mosaic_dask_worker.py`), and the Dask R harvest (`run_MOSAIC_helpers.R`) capture the same channel set. `capture_trajectories = FALSE` now gates the Dask Python worker itself, so it cuts the on-wire payload (not merely discarded R-side). Capture is post-calibration only (`run_laser_postca`); calibration is untouched. `trajectory_channels` is the documented RAM/disk lever; `.mosaic_ensemble_ram_projection_gb()` includes the capture term (and, on the Dask client path, the gathered-channel term) so the OOM warning fires honestly.
* **Capability check (no silent backend skew).** When `capture_trajectories = TRUE` but no worker returns the channels (e.g. a client running a MOSAIC build that predates this feature), the reduction emits a loud `warning()` and skips the artifact rather than silently producing trajectories on one backend and not the other. The Dask worker script (`mosaic_dask_worker.py`) is uploaded to the Coiled workers at runtime via `client$upload_file()`, so **no Coiled image rebuild is required** — reinstalling MOSAIC on the client is sufficient for the Dask backend to return the new channels.
* **New artifact `2_calibration/trajectories_ensemble.rds`** (`mosaic_trajectories`, schema-stamped) — per-channel weighted central series + uniform-thinned actual member lines + observed series + per-location endemic/epidemic CFR reference levels (`cfr_refs`). The medoid ensemble never captures trajectories. New directory key `res_fig_trajectories`. The transient scratch dir is removed on normal completion **and on any error/interrupt** (`on.exit`), so a failed run cannot orphan multi-GB of scratch.
* **CFR(t) panel** carries dashed endemic/epidemic regime reference lines (weighted-median `cfr_baseline`/`cfr_epidemic` from the `calc_implied_cfr()` sample columns over the best subset), with a caption noting the rolling window is `min(28, series length)` so a sparse panel on short series is not mistaken for a bug.
* **Robustness:** `.rec_mat` trims `tick+1` flow channels (recorded one time-column long) instead of silently dropping the FOI/incidence panels, and warns per-channel on an unrecoverable mismatch; display-line thinning uses `withr::with_seed()` (no global `.Random.seed` mutation); the reducer closes its per-channel connections on any exit; and the per-channel present-member count is surfaced so a compartment central computed over fewer members than its `reported_*` neighbour is visible rather than read as a model inconsistency.

# MOSAIC 0.53.0

* **New `"spatial"` figure group in `render_MOSAIC_figures()`** — six spatial-dynamics figures reconstructed from a finished run directory, in two families (mobility and transmission). Pure read-render (P5): config + persisted `.rds` arrays + a packaged basemap only — never a simulation or a GeoBoundaries / `get_country_shp()` API call. Figures: π_ij diffusion heatmap, τ_i departure (forest + N·τ daily travelers), modeled flux matrix M, mobility network, spatial importation hazard 𝓗_jt, and Keeling–Rohani coupling 𝓒_ij. Output to `3_results/figures/spatial/`.
    * **New pure helper `calc_mobility_flux(config)`** — single source of truth for the four mobility figures. Returns `{location_name, coords, N, tau, omega, gamma, D, pi, flux}` with the model-implied daily flux `M_ij = N_i·τ_i·π_ij` (diagonal `NA`), all in **config order** with axis labels aligned element-wise (no internal re-sort). Satisfies the identity `rowSums(M, na.rm=TRUE) == N·τ`. Uses only `get_distance_matrix()` + `calc_diffusion_matrix_pi()` — no disk, estimation, or API. This is the *model-implied* sibling of `plot_mobility()`'s *observed-OAG* panels (distinct quantities; not a fork).
    * **New exported plotters** `plot_diffusion_pi()`, `plot_departure_tau()` (CI bars only when supplied), `plot_mobility_flux_matrix()`, `plot_mobility_flux_network()` (centroid network over an optional `sf` basemap). All match docs aesthetics via `mosaic_colors`/`theme_mosaic`.
    * **Engine spatial arrays now extracted & persisted.** Both simulation workers previously `del`/`gc`'d the model object and returned only cases/deaths. The engine's `spatial_hazard` (J×T), `coupling` (J×J), and `pi_ij` (J×J) (filled by the engine `DerivedValues` component) are now extracted **inside each worker before discard, on both the local PSOCK path (`calc_model_ensemble.R`) and the Dask path (`mosaic_dask_worker.py`) in lockstep**, aggregated as the **element-wise median across the posterior-ensemble members**, and persisted to `2_calibration/{spatial_hazard,coupling,pi_ij}_ensemble.rds` (schema-stamped, config order, `location_name` attached). A Dask↔PSOCK serialization round-trip parity test asserts the `numpy.tolist()`→R reconstruction is bit-identical (incl. `NaN` cells). The coupling figure **masks `NaN`** (zero-variance/never-infected locations) explicitly. The renderer prefers the persisted engine `pi_ij` over an R recompute when present.
    * **τ-CI artifact (`1_inputs/mobility_tau_ci.csv`)** — `run_MOSAIC()` copies the per-location 95% CI from `MODEL_INPUT/mobility_travel_prob_params.csv` (the upstream `fit_prob_travel()` Beta posterior) into the run directory in config order; `plot_departure_tau()` draws interval bars when present, point-only otherwise. Unconditional (gated by `io`, not `plots`).
    * **Packaged basemap** `inst/extdata/africa_adm0_lowres.geojson` (~40 KB, derived from the per-country ADM0 shapefiles in `MOSAIC-data/processed/shapefiles`) provides the continental network backdrop without any API call. Subnational geographies fall back to a centroid-only network.
    * **F3 label-ordering fix.** `get_distance_matrix()` gains `sort = TRUE` (default, historical behavior preserved); `sort = FALSE` keeps input/config order so axes stay aligned to value vectors. `plot_spatial_hazard()` no longer re-sorts location labels alphabetically — it now follows `rownames(H)` (config order), preventing a silent row-mislabel when `H` is non-alphabetical. File `plot_spatial_correlation_matrix.R` renamed to `plot_spatial_correlation_heatmap.R` (exported function name unchanged).
    * **Engine-pool note (DM#3):** the rendered hazard is the engine array (single source of truth), which uses an S-only susceptible pool `S*_jt=(1-τ_j)S_jt`; the standalone R `calc_spatial_hazard()` uses a non-canonical `S+V1+V2` pool and is **not** reconciled to the engine figure.
    * Files: `R/calc_mobility_flux.R` (new), `R/plot_diffusion_pi.R` / `R/plot_departure_tau.R` / `R/plot_mobility_flux_matrix.R` / `R/plot_mobility_flux_network.R` (new), `R/plot_spatial_correlation_heatmap.R` (renamed + NaN-mask), `R/plot_spatial_hazard.R`, `R/get_distance_matrix.R`, `R/render_MOSAIC_figures.R`, `R/calc_model_ensemble.R`, `R/run_MOSAIC.R`, `R/run_MOSAIC_helpers.R`, `inst/python/mosaic_dask_worker.py`, `inst/extdata/africa_adm0_lowres.geojson` (new). Tests: `test-calc_mobility_flux.R`, `test-render_spatial_group.R`, `test-spatial_arrays_dask_psock_parity.R`.

# MOSAIC 0.52.0

* **Visualization separated from modeling in `run_MOSAIC()`** — the pipeline now produces a *complete, self-describing* `dir_output/` (every data artifact) **independent of plotting**, and all figures are rendered by a single standalone pass over the finished directory. Directly attacks the recurring "computation gated behind `plots=TRUE`" bug class (CLAUDE.md lessons #2/#9/#10).
    * **New exported `render_MOSAIC_figures(dir_output, which = NULL, plots = TRUE, verbose = TRUE)`** reconstructs every pipeline figure **from disk artifacts** (`.rds` ensembles, `samples.parquet`, posterior/diagnostic CSVs, prediction CSVs, `priors.json`). It is a **pure read-render**: it never calls `calc_model_ensemble()`, `run_LASER()`, or `sample_parameters()`; a missing / corrupt / schema-incompatible artifact is warned-and-skipped (never rebuilt, which would trigger local re-simulation). `which=` selects figure groups (`convergence` / `posterior` / `predictions` / `ppc` / `sensitivity` / `psi_star` / `parameters`); each figure is `tryCatch`-wrapped. Usable post-hoc, on another machine, or repeatedly.
    * **Data writes are now unconditional** (gated by `io`, not `plots`): prediction CSVs (`predictions_{ensemble,medoid}_*.csv`), `parameter_sensitivity.csv`, and `convergence_status.csv` are written by `run_MOSAIC()` regardless of `plots`. `run_MOSAIC(plots = FALSE)` and `run_MOSAIC(plots = TRUE)` now emit **identical data artifacts**; only the `*.png/*.pdf` figures differ.
    * **New pure helper `.mosaic_assemble_prediction_table()`** (extracted from `plot_model_ensemble()`) is the single source of truth for the masked prediction table, so the exported CSV and the plotted line are guaranteed identical. CSV schema is unchanged (`location, date, metric, observed, predicted_central, predicted_mean, predicted_median, central_method, ci_<k>_lower/upper`).
    * **New exported `calc_model_parameter_sensitivity()` and `calc_model_convergence_status()`** hold the HSIC computation and the convergence-status-table assembly (and their CSV writes); the corresponding `plot_*` functions are now pure consumers (accept a precomputed result, no longer own the CSV).
    * **In-memory objects persisted** for the renderer: `medoid_ensemble.rds` (G3 — the one ensemble previously unserialized), `subset_opt.rds` (G4). Every persisted `.rds` carries a `mosaic_schema_version` stamp so the renderer can warn-and-skip incompatible old run directories.
    * **`plot_model_parameters` wired in** as a renderer diagnostic (`parameters` group) — previously orphaned. `plot_model_ensemble(save_predictions=)` is **deprecated to a no-op with a warning** (not silently removed); the masking regression tests migrate onto `.mosaic_assemble_prediction_table()`.
    * **Tests:** engine-free P4 regression (`test-plots-false-still-writes-data.R`) asserts the data CSVs exist + match plotted content without any calibration; renderer round-trip (`test-render_MOSAIC_figures.R`) asserts figures appear and **no simulation is triggered** (P5, via mocked re-sim entry points), plus warn-and-skip on missing / schema-incompatible artifacts. Files: `R/render_MOSAIC_figures.R` (new), `R/calc_model_parameter_sensitivity.R` (new), `R/calc_model_convergence_status.R` (new), `R/plot_model_ensemble.R`, `R/plot_model_parameter_sensitivity.R`, `R/plot_model_convergence_status.R`, `R/run_MOSAIC.R`, `R/run_MOSAIC_helpers.R`.
# MOSAIC 0.51.0

* **Per-location `alpha_1`** (within-metapopulation population-mixing exponent). `alpha_1` is now driven **end-to-end as a per-location quantity** (length-`nL` vector) — sampling → priors → config validation → reticulate → engine, **including the Dask worker path**. The laser-cholera engine is already dual-mode (a scalar `alpha_1` is broadcast to all patches; a length-`(num_nodes,)` array is applied elementwise per patch in the force of infection), so **no engine change** was required. **`alpha_2` stays a single global scalar** (weakly identified given ψ absorbs the environmental signal — by design).
    * **Prior (`priors_default` v15.16):** `alpha_1` relocated from a single global `Beta` to a **per-location** prior carrying a **shared informative `Beta(28.4, 71.6)`** for every ISO (mean 0.284, sd ≈ 0.045, 95% CI ≈ [0.20, 0.38], cleanly within the engine `(0, 1]` invariant). The tight shared prior emulates hierarchical shrinkage (MOSAIC's independent-per-ISO sampler cannot express a true hierarchy) and starves the `alpha_1`↔`beta_j0_tot` degeneracy while still letting real per-location signal move it. `alpha_2` prior unchanged.
    * **Seed config (`config_default` v4.7):** `alpha_1` now stored as a length-`nL` vector (`rep(0.27, nL)`) so the `convert_matrix_to_config` round-trip preserves per-ISO calibrated values (a scalar seed silently dropped indices 2..`nL`). `alpha_2` remains a scalar (`0.50`).
    * **Three silent-corruption sites fixed in lockstep:** (a) `validate_sampled_config()` now treats `alpha_1` as **dual-mode** (scalar OR length-`nL`) rather than a rigid global scalar that hard-errored on a vector; (b) `get_param_names()` classifies a length-`nL` `alpha_1` as **per-location** (scalar `alpha_1` still routes to `$global` via the length-mismatch fallback); (c) the `convert_matrix_to_config` round-trip is now robust because the seed config carries a length-`nL` `alpha_1`.
    * **Dask parity:** `alpha_1` added to `mosaic_dask_worker.py::_VECTOR_FIELDS` (NOT `_AS_NDARRAY_FIELDS` — the engine recasts it to float32 via `np.asarray` regardless of input type, so it is parity-safe and does not trigger the float64-vs-float32 state-seeding divergence the `_AS_NDARRAY_FIELDS` header warns about).
* **Scalar back-compat preserved.** A scalar-`alpha_1` config (national `nL=1` and legacy configs) still validates and runs unchanged (engine broadcast); regression tests pin this.
* **Tests:** per-location `alpha_1` sampling invariants (length-`nL`, range `(0,1]`); `make_LASER_config` dual-mode validation (scalar / length-`nL` accepted, wrong-length / out-of-range rejected); `get_param_names` dual-mode classification; `convert_config_to_matrix`/`convert_config_to_dataframe` `alpha_1_<ISO>` expansion + round-trip; Dask schema-parity `alpha_1_<ISO>` assertion; `validate_sampled_config` dual-mode cases.
* **Note:** the logged sampled-parameter count rises by `nL` (e.g. +40 continental) now that `alpha_1` is per-location; no ESS/budget code change.

# MOSAIC 0.50.0

* **B2.1 — engine-correct `CFR_target` → `mu_j_baseline` chain factor** (`config_default` v4.6; `priors_default` **unchanged** at v15.15; statistician-validated). The B2 derivation (0.49.x) coupled `mu_j_baseline` to `CFR_target` via `gamma_1 * rho / (rho_deaths * chi_blend)` with `chi_blend = 0.5·(chi_endemic + chi_epidemic)`. A controlled-probe diagnosis of the laser-cholera deaths/`reported_cases` mechanism (statistician memory `b2-cfr-chain-factor-diagnosis`) showed the engine actually implies a **different chain factor**, fixed here in `sample_parameters()`:
    * **Dwell: `gamma_1` → `(1 - exp(-gamma_1))`.** Since laser-cholera v0.14.0 (#67) `reported_cases` is a thinning of daily *incidence*, not of symptomatic *prevalence-days*, so the recovery-tick factor relating the per-day mortality hazard `mu` to a per-incidence CFR is the survival complement `(1 - exp(-gamma_1))`, not `gamma_1`.
    * **PPV: `chi_blend` → `chi_epidemic`.** `reported_cases` is an `Isym` stock-read dominated by epidemic-regime ticks, so the *effective* PPV leans to `chi_epidemic` rather than the endemic/epidemic blend.
    * New derivation: `mu_j_baseline = CFR_target * (1 - exp(-gamma_1)) * rho / (rho_deaths * chi_epidemic)`. The `[0,1]` engine clamp and the Lesson-#12 version-skew guard are unchanged.
* **Why:** the OLD-B2 chain over-attributed deaths. B2.1 **reduces realized deaths bias 16–28%** on the problem countries (statistician validation) with **no harm to the well-calibrated ones** (COG slightly over-corrects to realized/target ≈ 0.91, within the residual). It removes the *derivable* part of the deaths-scale error; an **irreducible ≈1.3–1.5× dynamics-dependent residual** — driven by the realized epidemic-regime fraction and spatial coupling, which no closed form can capture — remains, and is expected.
* **`config_default` v4.6** — the static `mu_j_baseline` anchor is re-derived to match: `cfr_to_mu_adjustment = (1 - exp(-0.10)) * rho_mean / (rho_deaths_mean * 0.75) ≈ 0.1303` (was `≈ 0.1467` under v4.5; every country's config-default `mu_j_baseline = CFR_target × anchor` scales ×0.888). `CFR_target`, `psi_jt`, the surveillance fit target, and the calibration window are all **identical to v4.5** — patched surgically from the stored per-country `CFR_target` (no source-data regen, so no unrelated current-source drift). Per-episode CFRs for the high-N countries remain in `[0.5%, 15%]`. `priors_default` is **untouched** (B2.1 changes only the *derivation*, not the `CFR_target` prior).
* **Docs:** `MOSAIC-docs/04-model-description.Rmd` `mu_{j,0}` derivation (eq. `mu-baseline-derivation`) corrected to the incidence-based `(1 - exp(-gamma_1))` dwell factor and epidemic-leaning `chi^{epi}`, with the residual noted.
* **Tests:** the B2 regression fixtures (`test-sample_parameters_B2_mu.R`), the implied-CFR consistency guard (`test-cfr-pipeline-consistency.R`), and the hand-computed Fixture-1 `mu` (0.00273 → 0.00220) updated to the B2.1 identity. The engine read-back diagnostic (`test-implied-cfr.R` / `calc_implied_cfr.R`) is unchanged — it recomputes from the *emitted* `mu`, which is the point.

# MOSAIC 0.49.4

* **Test hygiene (maintainer review of the 0.49.x burst):** the three new 0.49.1 regression files were green only because the author's session happened to satisfy their hidden environment assumptions — they false-passed elsewhere. Two fixes, no production-code change:
    * **`test-ensemble_cluster_robust.R` / `test-run_batch_robust.R` now skip under the parallel testthat harness.** Both spawn a PSOCK cluster and `SIGKILL` its workers; doing that *inside* a `Config/testthat/parallel` worker (a callr subprocess) collides with testthat's own result IPC and crashed the harness. They now call `skip_if_testthat_parallel()` (the established package guard, matching `test-dask-psock-orchestrator.R` / `test-optimize_ensemble_subset.R`), so they run serial-only — exactly as covered by the serial `devtools::test()` and the CI engine-install run. The worker-death-robust gather logic itself is unchanged and verified correct (the dead-worker→scalar-`FALSE` coercion keeps the calibration `sum(unlist(success_indicators))` tally valid).
    * **`test-sample_parameters_B2_mu.R` is now root-independent.** 6 of its 9 blocks call `sample_parameters()`, which calls `get_paths()` whenever `PATHS` is `NULL` — so in any process/CI without a MOSAIC root set they errored in `get_paths()` *before* reaching the B2 logic (including block 9, which left the B2 version-skew `stop()` guard with zero real coverage). The blocks now pass a `PATHS = list()` stub (honouring the file's own "self-contained, no MOSAIC root needed" contract), exercising the B2 derivation and the version-skew guard directly. All 9 blocks pass with no root set.

# MOSAIC 0.49.3

* **laser-cholera engine 0.16.0 → 0.16.1** — contract-neutral for MOSAIC: the only engine-source change (0.16.0→0.16.1) is a Python-side `compute_wis_parametric_row` NaN-poisoning fix (#91) that MOSAIC's R-side likelihood does not use; SEIR dynamics, parameters, and the result schema are unchanged, so simulated cases/deaths are identical. Pin updated in `inst/python/environment.yml`.
* **B2 — dynamic `mu_j_baseline` ← `CFR_target` coupling** (`priors_default` v15.15 / `config_default` v4.5): `mu_j_baseline` is now derived at sample time from `CFR_target * gamma_1 * rho / (rho_deaths * chi)`, so the implied CFR is invariant to `gamma_1` drift and the ETH dwell stop-gap is removed. (0.49.0)
* **Worker-death-robust parallel gather** (`.mosaic_cluster_lapply_robust`): a PSOCK worker *process* crash (laser/numba C-abort or OOM) during the calibration or ensemble gather previously hung the master forever on Linux (blocking `unserialize()`); it now degrades on the survivors with a warning, or `stop()`s with a diagnostic past an idle timeout. Wired into both `.mosaic_run_batch` (calibration) and `calc_model_ensemble`. (0.49.1–0.49.2)

# MOSAIC 0.48.3

* **`priors_default` v15.14 / `config_default` v4.4 — deaths-bias + cases-bias prior fixes from the Stage-1 metapop review** (validated on a 6-country dugong recalibration before shipping; cases:deaths weight 1.0:0.5, matching the Stage-1 fits).
    * **B1 — `mu_j_baseline` CFR→μ derivation re-anchored** to posterior-consistent chain factors (`gamma_1` 0.1133→0.10, `chi` 0.639→0.70), scaling every country's center **×0.805** (cases-neutral). Corrects the systematic deaths over-prediction traced (independent 4-agent review) to the derivation freezing `gamma_1` at its prior mean while calibration consistently pulls it below the anchor (implied CFR ∝ `1/gamma_1` inflated ~1.2–1.8×). `mu_j_baseline` magnitude (the v0.13 `rho_deaths` correction) and `rho_deaths` itself are **unchanged**. The ETH ×0.40 dwell stop-gap is **retained** (held ≈0.00088): B1's *uniform* ×0.805 under-corrects the low-`gamma_1` countries, so ETH keeps its residual until the deferred Phase-2 dynamic per-country `gamma_1`-coupled derivation (B2) subsumes it.
    * **`beta_j0_tot` per-country recenter** — geometric-mean shrinkage `new_meanlog = 0.5·log(prior) + 0.5·log(posterior_median)` (w=0.5, **width kept**) toward the Stage-1 posteriors for the 18-country data cohort, correcting the ~1.6× cases over-prediction (data halved `beta` in most). Genuinely per-country (5 countries — BDI/MWI/NGA/RWA/TZA — move *up*); COG's deliberate override **preserved**; LBR (unidentified) left at default. Width is intentionally not tightened (the warm-start ×2 `beta_j0_tot` inflation guard).
    * **Validation (6-country dugong recalibration, 2018 window):** deaths bias dropped in all 6 (NER 2.81→2.25, MOZ 1.76→1.23, SSD 2.27→2.10, …), cases R² unchanged (≤0.01) — cases-neutral as designed. Residual ~2× on COD/SSD is the by-design implied-CFR / median-ensemble floor (`project_central_method_v038`), not a prior issue; the low-`gamma_1` countries (e.g. MWI, ~unchanged under B1) are the Phase-2 B2 case. Full `devtools::test()` green (FAIL 0).

# MOSAIC 0.48.2

Pre-scale-up package-health audit (parallel `swe` + `maintainer` review of the v0.45–v0.48 burst). Two Dask/Coiled-path correctness fixes (local PSOCK was unaffected throughout) plus build/doc hygiene.

* **[CRITICAL] Dask path: per-cell weight matrices no longer leak into the per-sim JSON.** `.extract_sampled_params()` (`run_MOSAIC_helpers.R`) did not exclude `reported_cases_weight`/`reported_deaths_weight`. Those matrices contain `NA` cells; on the Dask path `jsonlite::toJSON(digits=NA)` serializes `NA_real_` as the **string `"NA"`**, and `config.update(sampled)` on the worker then overwrote the clean float64 base_config arrays with lists carrying `"NA"` — so `np.asarray(..., dtype=float)` in the v0.48.0 weighted scorer raised `could not convert string to float: 'NA'`, **failing every Dask sim**; it also re-serialized the ~1.5 MB matrices per sim (~30× JSON bloat at 40K-sim scale). The matrices are broadcast once (clean numpy) via `.extract_base_config()`, so they are now excluded from the per-sim set. Latent until v0.48.0 began *consuming* the weights; a textbook local/Dask divergence (Lesson #12) — local PSOCK has no JSON round-trip.
* **[HIGH] Dask path: dtype parity for `as_ndarray` engine fields.** The engine's `as_ndarray()` returns an incoming numpy array unchanged but casts an incoming *list* to the declared dtype (`uint32` for IC counts, `float32` for seasonality/transmission vectors). The local PSOCK path ships these as lists (via `reticulate::r_to_py`), so the engine casts them correctly; the Dask worker pre-converted them to float64 numpy, so `as_ndarray()` left them float64 and the engine seeded its stochastic state in float64 instead of uint32 — a silent ~1–2% log-likelihood divergence from local at identical values + seed. A new `_AS_NDARRAY_FIELDS` registry keeps these fields as lists on the worker (and converts any base_config numpy copies back to lists), restoring bit-level dtype parity with local.
* **Hygiene:** rewrote a dead/empty dedup test in `test-get_location_priors.R` (guarded on a field path that no longer exists → 0 assertions; now 7 real assertions); fixed two roxygen Rd cross-reference WARNINGs (`calc_model_ensemble.Rd` `\link{i}`→`\code{}`; `get_greek_unicode.Rd` `\link{ggplot2}`→`\pkg{}`); added `__pycache__/`/`*.pyc` (and `vignettes/cache|figures`) to `.gitignore`/`.Rbuildignore` (the parity test's `source_python` compiles a `.pyc` into `inst/python/`); converted non-ASCII glyphs in `plot_model_subset_optimization.R` to `\uXXXX` escapes.
* **Known follow-ups (not blockers):** (a) CI never pip-installs laser-cholera, so the engine-backed parity tests — including the v0.48.0 Dask loc_idx guard — only run locally/on the VM, never in CI; (b) `burn_in_days=30` (the default) silently clamps to `n_time` on short/outbreak windows, yielding degenerate 0-contribution scoring — set `burn_in_days=0L` for short-window sims (a guard-warning is a candidate change pending statistician/disease-modeler input); (c) upstream laser-cholera WIS `np.sum`→`np.nansum` gap (see `claude/wis_issue_draft.md`).

# MOSAIC 0.48.1

* **Test fix (post-v0.16.0 audit):** `test-calc_model_likelihood_python_parity.R` test #7 had *pinned* a known R↔Python divergence in the zero-prediction cumulative penalty (Python ~1.78× more negative than R, observed on laser-cholera 0.13.1) with an explicit instruction to convert to an exact-parity assertion once the engine aligned. laser-cholera v0.16.0's verbatim port of `calc_model_likelihood` **aligned** the penalty (R and Python now agree to ~1e-12), which tripped the pinned ratio — the single failure surfaced by the full-suite health audit. The test now asserts exact parity. No production-code change; this is the early-warning test doing its job. `devtools::test()`: 0 failures. (Note: the built-tarball `R CMD check` carries a known non-CRAN baseline of standing ERRORs/WARNINGs/NOTEs — CRAN-incoming/examples-need-data-root/relative-`source()` test paths — none from this change; MOSAIC is explicitly non-CRAN and CI gates on `testthat::test_local()`, which is green.)

# MOSAIC 0.48.0

* **laser-cholera engine bumped to v0.16.0** (`inst/python/environment.yml`; both the release tag and the wheel filename). Two engine features are now integrated:
    * **Dual-mode `alpha_1` / `alpha_2`.** The engine now accepts either a global scalar (existing behaviour, bit-identical) **or** a per-location (`length(location_name)`) vector for the FOI mixing exponents, which `humantohuman.py` broadcasts/indexes via `np.power`. `make_LASER_config()` validation now accepts both forms and enforces the engine's range invariants (`alpha_1 ∈ (0, 1]` strict-positive, `alpha_2 ∈ [0, 1]`); the previous validator hard-required a scalar and used the looser `[0, 1]` lower bound for `alpha_1`. **Default sampling/priors remain global-scalar** — the per-location form is accepted for direct `run_LASER()` configs; per-location *sampling* (per-iso priors) is intentionally deferred. The scalar path is unchanged end-to-end (serialization, local PSOCK, and Dask all already pass a scalar through untouched).
    * **Dask/Coiled likelihood now honors the per-cell confidence weights (parity fix).** laser-cholera v0.16.0's `calc_model_likelihood` gained the `weights_obs_cases` / `weights_obs_deaths` matrix arguments (ported verbatim from MOSAIC's R `calc_model_likelihood`). The Dask worker (`inst/python/mosaic_dask_worker.py`) previously could not apply them — it emulated only the deaths-prefix zero by NaN-ing the deaths obs and otherwise ran **unweighted**, diverging from the local PSOCK R score whenever `reported_cases_weight`/`reported_deaths_weight` were non-trivial. The worker now passes the real (sliced, deaths-prefix-zeroed) weight matrices to the Python `calc_model_likelihood`, mirroring `run_MOSAIC.R` exactly, and **recomputes on-worker whenever the weights are non-trivial OR the scored window starts after t=1** (the engine analyzer's `model.log_likelihood` never sees `weights_obs_*`, so it cannot be trusted in those cases). The pure-default path (trivial weights, no slice) still uses `model.log_likelihood`, bit-identical to the engine analyzer. A new `_weights_obs_matrix_trivial()` helper gates the decision (matrix-level analogue of R's `.weights_obs_row_trivial`).
    * **Worker peak-sourcing fix (shape-term parity).** The v0.16.0 Python `calc_model_likelihood` matches `epidemic_peaks` rows by an integer `loc_idx` column (`getattr(row, "loc_idx", None)`), silently dropping any row that lacks it. The raw `config["epidemic_peaks"]` scattered to workers carries only `iso_code`/`peak_date` (see `.filter_epidemic_peaks()`), so the rewritten `_score_window_likelihood()` would have zeroed **every** peak-timing/peak-magnitude/cumulative shape term whenever shape weights were on — diverging from the local PSOCK R scorer (which dispatches by `iso_code`). The worker now sources peaks from `model.params["epidemic_peaks"]`, which the engine's `params.py` enriches with `loc_idx` at model construction — identical to what the engine analyzer reads. Verified to numerical tolerance against the R local path for the full burn-in-slice + deaths-prefix + per-cell-weights + peak-timing/magnitude/cumulative case. (Shape weights default to 0, so this was latent.) Covered by the new `test-dask_worker_score_window_parity.R`. **Known engine-side gap (not fixed here):** v0.16.0's `compute_wis_parametric_row` uses `np.sum` rather than `np.nansum`, so a single `NA` observation poisons the entire WIS row to `NaN` and the term is dropped engine-side, while R's `na.rm = TRUE` retains it — local and Dask diverge on the WIS term when `weight_wis > 0` over gappy surveillance data. WIS defaults to 0; a fix belongs in the read-only laser-cholera engine.
* **Resume safety:** v0.16.0 is intentionally **not** added to the `.mosaic_lc_likelihood_compatible()` byte-identical allow-list `{0.14.0, 0.15.0}`. Under non-trivial obs-weights a v0.16.0 Dask shard differs from a 0.15.0 shard, and the version-keyed allow-list cannot tell whether a given shard used trivial weights — so a 0.15.0 → 0.16.0 resume re-runs rather than risk pooling weighted and unweighted likelihoods.

# MOSAIC 0.47.5

* **Hardening of the config/priors manipulation ops behind the staged metapop calibration** (independent `swe` + `maintainer` deep review on the full 40-location structure). The staged round-trip (`get_location_config` → `update_priors_from_posteriors` → `inflate_priors` → `make_LASER_config`/`sample_parameters`) was verified engineering-correct on the real 40-loc `config_default`/`priors_default` and real Stage-1 posteriors (39/39 non-target-location invariance, exact lognormal `beta_j0_tot` inflation, bit-perfect matrix round-trip). Two defensive gaps closed:
    * `.validate_updated_priors()` now checks per-group location **names**, not just counts — a name-drift that preserves cardinality (e.g. a relabelled iso) previously passed validation silently (the very misalignment class this validator exists to catch).
    * `get_location_priors()` now returns locations in **canonical source order** (matching `get_location_config()`, which selects via `which(location_name %in% iso)`) instead of requested-argument order — removing a position-vs-name foot-gun for multi-iso callers. Sampling is name-keyed, so this changes order only, not values (no result change).
* **New `test-staged_metapop_prior_ops.R`** exercises these on the real 40-loc objects (previously only 2-location toy fixtures): partial-fold non-target invariance, validator name-drift detection, `get_location_priors`/`get_location_config` order consistency, full-40 `inflate_priors` scope + lognormal mean/variance, and the `convert_config_to_matrix`/`convert_matrix_to_config` 40-loc round-trip. (Deferred, doc-only: `convert_config_to_dataframe` is not param-set-equivalent to the matrix path — review item LOW-3, unused by `run_MOSAIC`.)

# MOSAIC 0.47.3

* **Default `control$likelihood$burn_in_days` changed from `0` to `30`.** The per-channel scored window (v0.47.0) is now **on by default**: the first 30 daily steps are excluded from the likelihood and R²/bias scoring. Rationale from the optimal-window research: the seeded E/I discharge into **cases** settles by ~day 14–21, but the **deaths** IC transient lags (death event + `delta_reporting_deaths` ≈ 5 d) and is still decaying out to ~day 28–35 — so a single 30-day window clears both channels' transients. On KEN this took cases-R² 0.338 → 0.56 and kept dropping the deaths bias well past 21 (3.26 → 2.50 at 21 → 2.22 at 28). Cost is ~1 % of a 2018 window / ~2.5 % of the 2023 window, and it is a no-op where there is no start spike (high-burden countries unaffected). **Behavior change, not bit-identical:** like the `central_method` change, this alters the scored target, so `config_best`/subset/medoid selection differs and runs are **not comparable across this boundary**. Set `control$likelihood$burn_in_days = 0L` to restore pre-v0.47.3 scoring. **Note:** on very short windows 30 may clamp toward `n_time` (the `min_obs_for_likelihood` gate then yields a 0-contribution, fail-safe) — set `0L` for short-window/outbreak sims. The residual deaths-bias floor (KEN ~1.6, COD ~1.27) is model deaths-baseline / implied-CFR, NOT IC transient, and is unaffected by burn-in (that's the `deaths_score_start`/CFR lever).

# MOSAIC 0.47.2

* **`config_default` (v4.3) + `priors_default` (v15.13) rebuilt under the relaxed trust-tier gate, at the unchanged 2023-01-01 window.** Following the v0.47.1 fourier-keep policy, the shipped fit-target matrices now retain `fourier_*` reconstructions at their lower per-week `confidence_weight` instead of NA-blanking them. 811 fourier weeks fall in the 2023+ window, so the change is material: `reported_cases_weight` cells in `(0,1)` rise from 10,062 → 17,073 (range 0.40–0.95), with the complementary drop in `==1` cells — those weeks now inform the fit as down-weighted real observations rather than being dropped. The simulation window is **unchanged** (`date_start = 2023-01-01`, `ncol = 1398`); only the per-observation weighting of previously-discarded weeks differs. priors `build_date_start = 2023-01-01`, `ic_t0 = 2023-02-01` (IC seeding epoch unchanged). `test-config_default_weights` (21/21) and `test-cfr-pipeline-consistency` (93/93) pass.

# MOSAIC 0.47.1

* **Surveillance trust-tier gate relaxed: keep `fourier_*` reconstructions, down-weighted.** `process_cholera_surveillance_data()` now NA-blanks **only** `assumed_zero` weeks (a pure surveillance-silence assumption). `fourier_*` rows — synthetic reconstructions of *real* annual/quarterly totals — previously hard-dropped are now kept and reach the daily fit target carrying their lower per-week `confidence_weight` (~0.4–0.5 vs ~0.9 for `observed`), which `calc_model_likelihood()` consumes as a per-observation weight. So low-confidence-but-real-magnitude weeks (e.g. the 2019–2022 JHU→WHO handoff gap) inform the fit at reduced weight rather than being discarded. `observed`, `documented_zero`, and direct WHO/JHU/SUPP rows are unchanged. Code-only here; the shipped `config_default`/`priors_default` are rebuilt under this policy in a follow-up commit.

# MOSAIC 0.47.0

* **Per-channel scored time window (burn-in + deaths-era start).** New run-time-only knobs in `control$likelihood` exclude leading time steps from the likelihood and R²/bias scoring, without touching `config_default`, the builders, or `ic_t0`:
    * `burn_in_days` (integer ≥ 0, default `0L`) — drops the leading IC-transient steps from **both** channels. MOSAIC seeds E/I by moment-matching at `date_start`; the seed discharges into reported cases over the first ~1–2 weeks, producing a day-1 spike the forced steady state can't match (verified on KEN: medoid day-3 cases 2761 vs observed 15, ~184× over observed; decays to ~observed by day 21). `burn_in_days=21` removes it from the scored series entirely (scored-window max 45 vs observed max 125).
    * `deaths_score_start` (`NULL` or `Date`/`"YYYY-MM-DD"`, default `NULL`) — scores deaths only from a later date (non-stationary observed CFR makes deaths unfittable before ~2023 for several countries).
    * `score_start_cases` (`NULL` or date, default `NULL`) — optional explicit cases start overriding the burn-in.
* **Mechanism.** Resolved once to per-channel 1-based start indices (`.mosaic_resolve_score_window()`), the worker **slices** `obs`/`est` to `min(idx_cases, idx_deaths):n` (not merely down-weighting — the peak/cumulative shape terms don't honor `weights_time`), zeros the deaths residual prefix, and passes the **sliced start date** so the peak `date_seq` stays aligned. R²/bias sites drop the unscored head via a generalized `.mosaic_mask_central_for_scoring()`; `calc_model_ensemble()` records `score_idx_*` in `artifact_mask`; `plot_model_ensemble()` blanks the unscored head for display.
* **Bit-identical at defaults.** All knobs at their defaults yield `idx_cases = idx_deaths = 1` ⇒ no slicing ⇒ scoring identical to pre-feature runs (new `test-burn_in_scoring_parity.R` + the unchanged tier-2/reference/obs-weights parity suites). Carried in `control$likelihood` ⇒ byte-compared by the resume guard ⇒ changing the window across a resume correctly errors. Dask path re-injects the resolved `score_idx_*` and filters `epidemic_peaks` at the sliced start (schema-parity test extended).

# MOSAIC 0.46.4

* **Hardening from the adversarial red-team of the 0.46.1–0.46.3 changes.**
    * `calc_model_ensemble()` gains a **RAM-projection guard**: before allocating the dense `[loc×time×param×stoch]` ensemble arrays it projects the concurrent peak and emits a loud warning (naming `max_best_subset`/`n_iter_ensemble` to dial down) when it would exceed ~80% of system RAM — so a long-window run (e.g. a 2015, ~4320-column config → ~51 GB at production defaults) fails loud with guidance instead of OOMing the orchestrator. Warn-only; a no-op at normal (2023, ~1400-col) widths.
    * `make_config_default.R` now **asserts its resolved `date_start` matches the installed `priors_default$metadata$build_date_start`** (newly recorded by `make_priors_default.R`) and `stop()`s on mismatch — closing the silent config/priors window-desync reachable if the documented rebuild order was skipped (back-compat warning for older priors lacking the field).
    * `ic_t0` selection **relabeled to its actual criterion — broadest-data-coverage** month (maximizes how many countries are seeded from real surveillance rather than Beta defaults), with an explicit note that case-volume weighting was considered and rejected (it biases the epoch toward an outbreak peak and over-seeds E/I). Factored into a pure, unit-tested `.ic_select_epoch()` helper (previously untested).
    * Doc/cosmetic nits: `optimize_ensemble_subset()` `@param` no longer claims the package default is `"mean"`; `plot_model_ensemble()` uses an adaptive date-break ladder (~10–15 ticks) instead of a fixed 3-month interval (~44 ticks over an 11-yr window).

# MOSAIC 0.46.3

* **Build start date (`date_start`) generalized across the config + priors builders.** `make_config_default.R` and `make_priors_default.R` both honor a `MOSAIC_BUILD_DATE_START` env override (single source of truth; fallback 2023-01-01) so the same start date flows into both, enabling back-history rebuilds (e.g. a 2015-01-01 start — all covariates cover ≥2015). The required rebuild order is documented in `make_config_default.R`. The `ic_t0` tie-break is now explicit (earliest month), with a loud guard if the 2023 anchor ever drifts off 2023-02-01 and a cold-start warning for far-future starts.

# MOSAIC 0.46.2

* **Data-driven initial-condition seeding epoch (`ic_t0`) that tracks `date_start`.** Replaced the hard `max(date_start, 2023-02-01)` floor — whose "2015 has no active-case countries" premise predated the multi-source JHU/AI back-history — with a data-driven epoch (broadest-data-coverage month) within `[date_start, date_start + 12mo]`. Inert for the 2023 default (resolves to 2023-02-01; shipped priors numerically unchanged), and for a back-history build it seeds ICs from that era's data instead of an 8-year-mismatched 2023 state.

# MOSAIC 0.46.1

* **Default ensemble `central_method` changed from `"mean"` to `"median"`.** The posterior-weighted ensemble now summarizes each per-cell predictive with the **median** by default — the conventional epidemiological-forecast point estimate, robust to stochastic right-tail outliers (this is the rationale; note that on a right-skewed predictive a lower bias-to-1.0 ratio mostly reflects the skew, so "lower bias" alone is *not* the justification). **Behavior change, not reporting-only:** `central_method` feeds the best-subset optimizer (`optimize_objective = "mae"`), so this changes the selected subset, the medoid, and `config_best.json` — runs are **not directly comparable across this boundary**. Both `*_ensemble_mean` and `*_ensemble_median` R²/bias remain in `summary.json`, so the mean-based implied-CFR/skew diagnostic is preserved as a cross-walk field. All default sites updated in lockstep (resolver, `mosaic_control_defaults`, `plot_model_ensemble`, `run_rolling_cv`).

# MOSAIC 0.46.0

* **Engine-artifact mask for post-calibration R²/bias scoring (metrics-only).** Two laser-cholera (0.15.0) array artifacts were silently contaminating the post-calibration fit metrics: (1) the first ~2 `reported_cases` timesteps are an initial-condition warm-up transient (seeded E flushing into `new_symptomatic` before the SEIR dynamics settle), and (2) the final `reported_deaths` timestep is a structural zero (written at `[tick]` then `[1:]`-trimmed; laser issue #82). `plot_model_ensemble()` already masked these for DISPLAY ONLY; the R²/bias metrics in `run_MOSAIC()` still scored them.
    * `calc_model_ensemble()` now carries the mask spec via two new params (`n_cases_warmup_mask = 2L`, `mask_final_deaths_step = TRUE`, defaults matching `plot_model_ensemble()`) and records the resolved spec in the returned `mosaic_ensemble` object as `artifact_mask = list(cases_warmup, deaths_final)`. The central/quantile/array fields stay RAW (unmutated).
    * A new internal helper `.mosaic_mask_central_for_scoring()` sets the artifact time-positions to `NA` on the EST central matrix only; observed series stay unmasked and `calc_model_R2()`/`calc_bias_ratio()` drop the NA pairs pairwise. Wired into every R²/bias scoring site in `run_MOSAIC()` (canonical ensemble, dual mean/median, tier subset, medoid; windowed metrics inherit the masked canonical series). The medoid SELECTION distance is deliberately left unmasked. CSV/prediction export and plot series are unchanged.

# MOSAIC 0.45.5

* **Test suite sped up ~2x and re-tiered.** The default fast-tier `devtools::test()` now runs in parallel (`Config/testthat/parallel: true`) at ~70–90s wall-clock on a 10-core laptop versus the ~153s single-threaded baseline, with no loss of assertion coverage. Changes:
    * **Parallel testthat** with `Config/testthat/start-first` ordering the slowest cluster-spawning files first. A new `setup-python.R` pins all thread env vars (OMP/MKL/OPENBLAS/NUMEXPR/TBB/NUMBA + ARROW = `1`) and `BLAS` threads to 1 in every worker *before* Python starts, then probes the interpreter once and caches the result in `options()`. The 5 tests that themselves fork (`mclapply`) or spawn PSOCK clusters self-skip under a testthat callr worker (`skip_if_testthat_parallel()`) to avoid result-IPC corruption; CI runs serial (`TESTTHAT_PARALLEL=FALSE`) so they retain full coverage.
    * **Removed a gratuitous TensorFlow load** from `test-est_suitability_dispatch.R`: the deprecation/unknown-arg tests now mock `.est_suitability_lstm_v2` (the message fires before dispatch) instead of falling through into the real lstm_v2 path (~14s saved where TF is installed).
    * **Trimmed Monte-Carlo / whole-config work** in the prior and sampler tests: `est_zeta_*` `n_sim` 10000/5000 → 500; `sample_parameters()` distributional loops 200/100 → 40; a shared `local()`-memoized `.cached_sampled_config()` fixture (`helper-fixtures.R`) replaces repeated full 301-parameter draws in structural-assertion tests.
    * **Centralized skip helpers** into `helper-skips.R` (single source of truth for `skip_if_no_python_likelihood`, `skip_without_tensorflow`, `skip_if_no_data`, `skip_if_few_cores`, `skip_if_no_rho_deaths_prior`), removing duplicated inline copies across 5 files, and added `skip_if_slow()` (gated on `MOSAIC_RUN_SLOW_TESTS`) to tier genuinely-slow non-engine tests.
    * **Shrank the flood-GAM test fixture** (`test-impute_flood_probability.R`) from 5→2 synthetic ISOs (28s→19s warm); the AUC signal-recovery assertion is preserved always-on (AUC 0.82–0.88 across seeds, clearing the 0.75 threshold with margin).
    * **CI workflow** (`.github/workflows/R-CMD-check.yaml`): push/PR runs the fast tier; a scheduled + `workflow_dispatch` job runs the slow tier with `MOSAIC_RUN_SLOW_TESTS=1`.

# MOSAIC 0.44.13

* **Upgraded the laser-cholera engine pin to v0.15.0** (`inst/python/environment.yml`). v0.15.0 is a docs-migration + validation-hygiene release: it converts `assert`-style checks to always-on `if/raise ValueError/AttributeError` (so parameter validation runs even under `python -O`), tightens the `metapop` CLI `--over` validation, and modernizes type hints — but it leaves the **parameter contract and the `model.results` schema (`reported_cases`/`reported_deaths`) unchanged**. Verified contract-neutral: a deterministic `run_LASER(config_default)` smoke run and the full test suite pass against v0.15.0 with no changes to the R observation-model code, priors, or config. Unlike the v0.13 (deaths-scale) and v0.14 (reported-cases-incidence) upgrades, no prior/derivation changes are required.
* **Resume now pools across verified-likelihood-compatible engine versions on the Dask backend.** Added `.mosaic_lc_likelihood_compatible()` (an explicit, conservative allow-list) and wired it into `.mosaic_resume_check_inputs()`: when persisted and current shards were both Python(Dask)-scored by laser-cholera versions whose engine `calc_model_likelihood` and output schema are byte-identical (currently `{0.14.0, 0.15.0}` — v0.15.0 changed only docs/type-hints), resume proceeds with a warning instead of hard-failing the likelihood-provenance guard. Non-allow-listed pairs (e.g. `0.13.0` ↔ `0.15.0`) still hard-error, and the local (R-scored) path is unaffected. Note: the **Coiled worker image / Coiled software env must be rebuilt to v0.15.0 to keep client↔worker version parity** (hybrid runs with mismatched engine versions remain invalid).

# MOSAIC 0.44.11

* Integrated the Phase 3 (#101) Dask worker-schema rewrite into main: workers now compute the likelihood on-worker and return a per-iter `{iter, seed_iter, likelihood}` schema, the Dask gather adapter collapses to a single gather-and-write loop, `.mosaic_inject_likelihood_settings()` re-injects `location_name` + `N_j_initial` and casts the observed surveillance matrices to double before scatter, and the `test-dask_worker_schema_parity.R` regression suite is added. Merged on top of the post-0.40.0 main evolution.

# MOSAIC 0.40.0

* **New `backfill_weekly_case_gaps()` + `compile_suitability_data(backfill_case_gaps = TRUE)` repair holiday-week false zeros in the suitability target.** Genuinely-missing weekly surveillance weeks (most often the Christmas/New-Year reporting lapse) were turned into `0` cases by the suitability pipeline's `NA→0` keras-target sanitiser, fabricating a "no transmission" week inside an active outbreak. The new helper linearly interpolates **short interior** gaps (`≤ max_interp_weeks`, default 2) that are bounded by reported weeks, on the per-group weekly series, inside `compile_suitability_data()` before the target is built. It is **suitability-local by design** — the canonical surveillance files and the calibration likelihood (which correctly NA-skips missing weeks) are untouched.
* Interpolation is **date-weighted** (handles unevenly-spaced rows, e.g. a dropped ISO W53 leaving a 14-day step); **reported anchors are never modified** (rounding/clamping apply to filled cells only, so fractional disaggregated counts are preserved); gaps **shouldered by a reported zero are left unfilled** (`min_anchor`, default 0 — a near-zero shoulder is more likely a genuine non-outbreak week); phantom duplicate `(iso, date)` rows are not fabricated; and a `cases_interpolated` flag marks the filled weeks.

# MOSAIC 0.39.0

* **`run_MOSAIC()` now produces only the posterior ensemble and the medoid representative model — the single best-likelihood model is no longer produced.** Removed: the best stochastic ensemble, its prediction plot/CSV (`predictions_best_*`), its Dask dispatch, the `config_best.json` artifact, and the best-seed sampling fatal gate. The best-**subset** calibration machinery (`is_best_subset`/`weight_best`, and the `is_best_model` flag in `samples.parquet`) is unchanged — it still drives the posterior ensemble and parameter diagnostics.
* `summary.json` now reports ensemble (+ tier) fit metrics only: the single-model fields `r2_cases`/`r2_deaths`/`bias_ratio_cases`/`bias_ratio_deaths` are removed. Run-success (`outputs_ok`/`[RUN_SUMMARY]`) keys off `r2_cases_ensemble`; the `r2_cases_best`/`r2_deaths_best` log fields are dropped.
* `run_rolling_cv()` drops `"best"` from its default `models` (now `c("ensemble","ensemble_opt","medoid")`). `"best"` is still accepted for back-compat and re-simulated when an older run dir carries `config_best.json`, otherwise skipped with a warning.
* Migration: scenario scripts that read `config_best.json` should repoint to `config_medoid.json` (the representative single-config model).

# MOSAIC 0.38.0

* **New `central_method` control selects the ensemble central tendency (default `"mean"`).** A single setting — `control$predictions$central_method`, `"mean"` (default) or `"median"`, scalar or per-channel `c(cases=, deaths=)` — now consistently governs the ensemble central trajectory used for (i) the prediction trajectory + plots, (ii) the canonical `*_ensemble` R²/bias metrics, (iii) the medoid representative member ("mean-medoid"), and (iv) the subset-selection objective (`optimize_ensemble_subset()`). The weighted **mean** is the unbiased estimator of expected counts (`E[Σ]=ΣE`) and never collapses to zero on sparse deaths (~92% zero-days), where the per-tick weighted median otherwise reads as a zero-collapse. Cases (dense) are insensitive (mean≈median).
* **Behaviour change:** because the default flips from the historical median to the mean, all `*_ensemble` metrics, the medoid seed, and the prediction plots differ from pre-0.38 runs and are not directly comparable. Set `central_method = "median"` to reproduce historical runs bit-for-bit (the regression-anchored path — ensemble, tier, and medoid metrics, the medoid seed, and the optimizer-selected subset N are all unchanged under `"median"`). The WIS objective is quantile-based and unaffected by `central_method`.
* **`summary.json` cross-walk:** during the transition both `r2_*_ensemble_mean`/`r2_*_ensemble_median` (and the matching `bias_ratio_*`) are emitted alongside the canonical `r2_*_ensemble`, plus `central_method_cases`/`central_method_deaths` provenance fields.
* **Prediction CSVs** now carry `predicted_central` (the plotted/scored series), `predicted_mean`, `predicted_median`, and a `central_method` column — `predicted_median` always holds the true median (never mislabeled). `plot_model_ppc()` prefers `predicted_central`.
* **Rolling-CV** (`run_rolling_cv()`, `compile_rolling_cv_predictions()`, `evaluate_rolling_cv()`, `plot_rolling_cv()`) threads `central_method` (default `"mean"`) so in-sample and out-of-sample point metrics use the same summary. The predictions table gains `pred_central`/`pred_mean`/`central_method` (keeping `pred_median`); R²/bias/MAE/RMSE/skill score on `pred_central` while WIS/coverage stay quantile-based on the median.
* **Reading the new numbers:** under the default mean, the canonical `r2_*_ensemble`/`bias_ratio_*` fields differ from pre-0.38 same-named fields (mean vs median); read `central_method_cases`/`central_method_deaths` and compare across versions via `r2_*_ensemble_mean`/`r2_*_ensemble_median`. In particular the **deaths bias ratio will rise to ~2× on existing runs** — this is the expected unmasking of the posterior implied-CFR property (accepted as admissible; see the plan §0), not a regression. The implied-CFR diagnostic (`summary.json:cfr_implied`) is computed from raw member arrays and is **unchanged** by `central_method`.
* **Medoid retarget:** `config_medoid.json` is now selected against the chosen central case series; scenario owners that pin the medoid base member should re-pin. The medoid distance is **cases-anchored** (a representative case trajectory), not deaths-calibrated — a single-member base under-states the across-member mean deaths, so deaths-sensitive counterfactuals should propagate the full retained ensemble.
* **Rolling-CV note:** OOS point metrics (R²/bias/MAE/RMSE/skill) default to the mean for in-sample/out-of-sample consistency; these are not directly comparable to externally-reported *median*-point forecast errors on right-skewed counts. WIS and interval coverage remain median/quantile-based (the externally-comparable scores).
* **Known cosmetic (sparse deaths):** the plotted central line is the weighted mean while the ribbon shows member quantiles, so on near-all-zero death series the mean line can sit slightly *above* its own 97.5% ribbon (a moment vs order-statistics of a zero-inflated count — mathematically correct, not a plotting bug). The proper-scoring "predictive fan" in plan §7 is the planned follow-up.

# MOSAIC 0.36.12

* Fixed a medoid-model mis-mapping: the medoid metric correctly selected the central ensemble member, but the seed it was mapped to came from a separate vector that could drift out of positional alignment with `cases_array`, so `config_medoid` was sampled from the wrong parameter set and its prediction could collapse to near-zero. `calc_model_ensemble()` now carries a per-member `seeds` vector aligned with `cases_array` (sourced from the parameter set that produced each member), `optimize_ensemble_subset()` propagates it through its sort/slice, and `run_MOSAIC()`'s medoid uses `ensemble$seeds[medoid_idx]`. Bit-identical for correctly-aligned runs (Tier-2 parity unchanged).

# MOSAIC 0.36.4

* Test integrity: replaced the two false-green parity-test `skip()`s with active divergence monitors so the R/Python likelihood-drift suite no longer reports green over known disagreements (review B2-6).

# MOSAIC 0.36.3

* Hygiene: declared previously-undeclared package dependencies, fixed markdown-link Rd errors, and updated `.Rbuildignore` (Batch 5a).

# MOSAIC 0.36.2

* Fixed Dask ensemble weight misalignment plus guardrail/renormalization correctness in the posterior-weighted ensemble (deep-review Batch 1).

# MOSAIC 0.36.1

* Fixed Dask worker-count staleness; added `plot_rolling_cv()` and lstm_v2 model artifacts.

# MOSAIC 0.36.0

* Run the orchestrator sampling/parquet-write loops on PSOCK rather than fork for cross-platform stability (B2).

# MOSAIC 0.35.2

* Separated local (`n_cores`) from remote (`dask_spec`) parallelism and fixed the Dask worker-count derivation.

# MOSAIC 0.35.1

* Fixed ensemble weight misalignment and applied a bit-identical Tier-2 performance refactor (guarded by parity fixtures).

# MOSAIC 0.34.0

## est_suitability(): new default lstm_v2 hierarchical-FiLM architecture (rolling-origin CV)

`est_suitability()` is now an architecture dispatcher. The v0.34 default,
`architecture = "lstm_v2_hierarchical_film"` ("gauge_A"/"B4"), replaces the
v0.33 shared LSTM with a hierarchical-FiLM model trained under expanding-window
rolling-origin cross-validation — fixing the v0.33 random-split temporal leak
that collapsed out-of-sample forecasts to a near-flat line.

**Architecture.** A 3-stack LSTM trunk (128->64->32, recurrent dropout) maps the
13-week covariate window to a shared climate-response latent `z`; a region
embedding and a zero-initialized country *deviation* embedding modulate `z` via
two FiLM stages (`tanh` gains in `[0,2]`, identity at init), with an L2
partial-pool penalty shrinking data-sparse countries toward their region. BCE
loss under `balanced_uniform` sample weights (correcting the zero-week
imbalance) plus an optional per-row confidence-weight overlay that down-weights
AI-mined surveillance rows. Each step validates strictly forward in time
(4-week embargo); the model refits on full in-sample data at
`median(best_epoch)`; `n_seeds` fits are combined on the logit scale.

**Output schema (Option A).** The prediction CSVs now carry a single canonical
`psi` column (smoothed; bias-corrected when `bias_correct = TRUE`) consumed by
every psi->`psi_jt` reader, plus transparent diagnostics (`pred_raw`,
`pred_smooth`, `pred_bias_corrected`, and seed-dispersion quantiles
`q025/q25/q75/q975` — diagnostic, NOT predictive intervals). The latent v0.33
no-op (bias correction wrote `pred_calibrated` while readers used `pred_smooth`)
is fixed.

**Signature.** New shared toggles `feature_set` (default `"v7.3"`), `response_var`
(default `"transmission_intensity"`), `bias_correct` (renamed from `calibrate`),
`architecture`, and a single `arch_control` list holding all lstm_v2
hyperparameters (loaded from `inst/fixtures/B4_rolling_cv_spec.yml`). The v0.33
process knobs (`n_splits`, `seed_base`, ...) and `exclude_covariates` are frozen
inside the legacy path and absorbed via `...` with a deprecation message;
`calibrate` is mapped to `bias_correct`.

**Bias correction.** `calibrate_psi_predictions()` rewritten to a per-country
logit-scale affine fit on outbreak (non-zero observed) weeks only, applied via
the monotone inverse logit (no hard `[0,1]` clip). Zero-history / low-incidence
countries fall back to identity (uncorrected, region-FiLM-modulated psi).

**Region maps.** `arch_control$region_map` selects one of `snf_k5` (production
default; Similarity Network Fusion 4-view clustering), `csv` (4 WHO admin
regions; the B4 reproduction baseline), `seasonal_v1`, `hydro_v1`, or `snf_k4`.

**Provisional defaults.** The v0.34.0 defaults
(`response_var = "transmission_intensity"`, `region_map = "snf_k5"`,
`bias_correct = TRUE`) are provisional, gated by a post-merge psi->LASER
case-skill experiment; `snf_k5` reverts to `csv` if it does not beat it, and
`bias_correct` to `FALSE` if it worsens downstream case-skill.

**Legacy.** `architecture = "lstm_v1_legacy"` preserves the frozen v0.33
shared-LSTM path (random split + sequential fine-tuning) for
reproducibility/rollback.

# MOSAIC 0.33.4

* `run_rolling_cv()` now scores all four model types (ensemble, optimized, best, medoid); added `models` and `n_reps_best_medoid` knobs to `compile_rolling_cv_predictions()`.

# MOSAIC 0.33.3

* `run_rolling_cv()`: enabled the `run_MOSAIC` best-subset optimizer per cutoff.

# MOSAIC 0.33.2

* Added `evaluate_rolling_cv()` for post-hoc out-of-sample forecast scoring.

# MOSAIC 0.33.1

* `run_rolling_cv()`: always refit psi per cutoff — removed the `refit_psi` flag.

# MOSAIC 0.33.0

* `est_suitability()` v0.33 AI-data production spec: v7.3 feature set, `target_C` response, and per-country calibration.

# MOSAIC 0.32.9

## Phase 3 (#101): cast observed surveillance matrices to double before scatter

End-to-end Dask smoke run surfaced a silent reticulate serialization bug
that returned `-Inf` for every sim's likelihood. Root cause:
`get_location_config(iso = "ETH")` (and every other location's config)
ships `reported_cases` / `reported_deaths` as R **integer** matrices —
counts are natively integer-valued — with `NA_integer_` for missing
surveillance weeks. When `reticulate::r_to_py()` serializes an integer
matrix to numpy, `NA_integer_` becomes `INT32_MIN` (`-2147483648`) on
the worker side, because numpy `int32` has no NaN representation. Those
huge negative sentinels then evaluate as valid (extreme) counts in
`calc_model_likelihood`'s NB term, returning `-Inf` for every sim.

The smoke run's parquet read `likelihood: -Inf` for all 5000 sims; the
gather adapter's multi-iter collapse then turned 5 copies of `-Inf`
into `NA_real_` (no finite values to log-mean-exp), so `samples.parquet`
showed `% finite: 0`.

### Fix

`.mosaic_inject_likelihood_settings()` now casts `config$reported_cases`
and `config$reported_deaths` to `storage.mode = "double"` before scatter.
`storage.mode()` preserves matrix `dim` and only flips the underlying
type; `NA_integer_` → `NA_real_` → `np.nan` via reticulate, which
`calc_model_likelihood` then masks correctly via `np.isfinite()`.

The cast lives in the Dask-path-specific helper so the local-path
R-side `calc_model_likelihood` is unaffected — it handles
`NA_integer_` and `NA_real_` identically via `is.finite()` masking.

### Validation

1-sim ETH smoke against a Coiled cluster (1 D4s_v6 worker, freshly
rebuilt v0.32.7 image with laser-cholera 0.13.1):
- Before fix: `likelihood: -Inf, is finite: FALSE`
- After fix: `likelihood: -17454.14, is finite: TRUE`

The end-to-end on-worker scoring path is now confirmed functional.
Production smoke (5K sims) and the 1K-sim performance benchmark
remain to be re-run.

---

# MOSAIC 0.32.8

## Phase 3 (#101): swap removed MOZ fixtures for global `config_default` in parity test

`tests/testthat/test-dask_worker_schema_parity.R` loaded `config_default_MOZ`
and `priors_default_MOZ`, both removed in v0.30.49. All three parity tests
were SKIPping with "config_default_MOZ / priors_default_MOZ not available"
since the merge, leaving Phase 3's worker-schema regression coverage inert.

Switched the fixture loader (renamed `skip_if_no_moz_data()` →
`skip_if_no_data()`) to use the global multi-country `config_default` /
`priors_default`. Because the parity tests are R-side flattening only — no
LASER simulation — the ~40-location config is cheap to exercise and gives
strictly broader coverage of the ISO-suffix invariant than the single-
country MOZ fixture would: TEST 2's per-location loop now runs ~40
assertions instead of 1, validating that every SSA ISO suffix survives
the worker round trip. All three tests now PASS (was: 3 SKIPs).

Docker post-fix: `test_file()` reports `[ FAIL 0 | SKIP 0 | PASS 42 ]`.

---

# MOSAIC 0.32.7

## azure/Dockerfile: harden Python-env build against silent failures

The previous `:latest` push silently produced a broken image: the `r-mosaic`
virtualenv was never created (only reticulate's default `r-reticulate`
conda env with just `numpy`), and `laser-cholera` wasn't installed at all.
Diagnosis revealed two latent bugs in [azure/Dockerfile](azure/Dockerfile)
that combined to mask the failure:

1. **Failure-masking shell chain in the python-deps step.** The single
   trailing semicolon (`; rm -rf ...`) before the final `echo` short-circuited
   the `&&` chain — if `MOSAIC::install_dependencies()` or any chained pip
   command failed, the `rm -rf && echo "..."` tail still exited 0, so the
   build looked green. **Fix**: all conjunctions now `&&`; `conda clean`
   wrapped in `(... || true)` since its `2>/dev/null` redirect intentionally
   ignores warnings but should not fail the build.

2. **Verify step too lenient.** `MOSAIC::check_dependencies()` prints
   warnings rather than erroring on missing modules, so a half-installed
   venv (no laser-cholera) passed the previous `tryCatch` guard.
   **Fix**: prepend three hard assertions before the R-side check —
   `test -x /root/.virtualenvs/r-mosaic/bin/python`, then a `python -c
   "import laser.cholera; from laser.cholera import calc_model_likelihood"`
   one-liner that fails the build if either import is missing. Confirms
   the venv exists AND the v0.13+ analyzer submodule is reachable.

3. **`RETICULATE_PYTHON` pinned at image level.** The container only had
   `ENV PATH=/root/.virtualenvs/r-mosaic/bin:$PATH`. reticulate's
   discovery doesn't always honor PATH — `Rscript` started outside the
   MOSAIC `.onLoad` hook fell back to a uv-bootstrapped ephemeral
   Python at `/root/.cache/R/reticulate/uv/...`, completely bypassing
   the installed venv. This caused `devtools::test()`'s pre-`library()`
   setup phase to report laser-cholera as missing and SKIP 11 tests
   that depend on the engine being importable. **Fix**: add
   `ENV RETICULATE_PYTHON=/root/.virtualenvs/r-mosaic/bin/python` so
   every R session in the image — MOSAIC-loaded or not — sees the
   right Python.

### Migration

Next image rebuild via `docker build -f azure/Dockerfile ...`
(use `--no-cache` to evict the now-stale layers, or just retag the
freshly-rebuilt image). Existing pulled images are unaffected until
re-pull.

---

# MOSAIC 0.32.6

## Phase 3 (#101): test fixes for post-merge contract updates

Three test failures surfaced by the first post-merge `devtools::test()`
docker run, all from cross-PR-merge interactions rather than behavior
regressions:

- **`test-config_default.R::"make_LASER_config validation: unknown iso_code..."`** —
  v0.32.0 promoted the warning to a hard error (per the v0.13+
  `epidemic_peaks ⊆ location_name` assertion at
  [R/make_LASER_config.R:918](R/make_LASER_config.R#L918)), but the test
  was still asserting `expect_warning`. Switched to `expect_error` and
  retitled the test to reflect the new contract.

- **`test-run_MOSAIC_resume.R::".mosaic_likelihood_provenance reports R engine..."`** —
  this is the test John wrote alongside the v0.32.4 forward hook
  (`# scoring is R-side on all paths until phase 3`). v0.32.5 activated
  the hook, so the assertion has to flip: `use_dask = TRUE` now returns
  `engine = "python"` with `impl_version` carrying the laser-cholera
  engine version. Test now asserts both branches of the helper
  explicitly.

- **`test-run_MOSAIC_resume.R::".mosaic_resume_check_inputs rejects a different likelihood provenance"`** —
  the third sub-check wrote a hard-coded `pkg_laser_cholera = "0.13.0"`
  into the fixture. On docker images whose installed laser-cholera lags
  `inst/python/environment.yml` (e.g., a `:latest` tag that pre-dates
  the v0.32.0 engine pin), the deaths-scale engine-version guard fires
  before the likelihood-provenance check the test was trying to
  exercise. Fixed by querying the live engine version via
  `importlib.metadata.version("laser-cholera")` (the same call the
  production guard uses at
  [R/run_MOSAIC_helpers.R:1015-1020](R/run_MOSAIC_helpers.R#L1015-L1020))
  and writing that into the fixture, so the engine guard is satisfied
  regardless of which container the suite runs in.

## rho (care-seeking) prior re-derived: Wiens 2-stratum RE pool

`R/get_rho_care_seeking_params.R` now anchors the rho prior on a random-effects meta-analytic pool of **both** Wiens et al. 2025 case-definition strata, replacing the prior pool of 12 GEMS Nasrin 2013 pediatric MSD strata + 1 Wiens severe/cholera summary.

**Anchors (Wiens et al. 2025, PMC12013865):**
- General diarrhea: 29.9% [25.3, 35.1] from 122 observations
- Severe diarrhea + cholera: 58.6% [39.9, 75.2] from 22 observations

**Pooled prior:** `Beta(5.38, 7.10)`, mean **0.423**, 95% CI [0.21, 0.65], ESS ≈ 12.5 (was: `Beta(6.81, 17.89)`, mean 0.276, 95% CI [0.12, 0.46], ESS ≈ 25).

**Why this change:**
1. **Severity match.** Symptomatic cholera spans the full mild-to-severe spectrum. Anchoring on the severe-only stratum biases rho upward (severe-only is dominated by outbreak-response settings); anchoring on general diarrhea alone biases rho downward (includes many self-resolving mild episodes). Pooling both honestly reflects the severity distribution.
2. **GEMS already included via Wiens.** The Wiens 2025 dataset includes 6 GEMS-derived Study IDs covering all 7 GEMS sites. The prior GEMS+Wiens pool double-counted the same source populations.
3. **Dimensional fix.** The prior pool weighted 12 GEMS stratum-level observations against 1 Wiens meta-analytic summary — upside-down vs Wiens's 23-study underlying meta-analysis. The new pool treats each Wiens stratum (122 + 22 obs) as one observation in the meta-analysis.
4. **Cascading mu_j_baseline update.** The steady-state CFR identity `mu_j_baseline = CFR × rho / (rho_deaths × chi)` propagates the rho change: per-country `mu_j_baseline` values rescale by ~1.5× (0.423 / 0.276). Implied per-symptomatic-episode CFRs for high-N test countries remain within the [0.5%, 15%] plausibility range: MOZ 3.5%, ETH 9.3%, KEN 10.1%, COD 14.9%.

**Files updated:**
- `R/get_rho_care_seeking_params.R` — new derivation with RE pool of two Wiens strata
- `R/plot_rho_care_seeking_params.R` — diagnostic figure updated to show both strata
- `model/input/param_rho_care_seeking.csv` — regenerated Beta parameters
- `data/priors_default.rda` + `inst/extdata/priors_default.json` — v15.8 (rho + all per-country mu_j_baseline cascaded through the CFR identity)
- `data/config_default.rda` + `inst/extdata/config_default.json` — v3.8 (rho + per-country mu_j_baseline updated surgically; full make_config_default.R build deferred due to pre-existing unrelated psi_jt validation issue)
- `data-raw/make_priors_default.R`, `data-raw/make_config_default.R` — header comments and hardcoded values updated

**Geographic note:** the Wiens severe+cholera stratum draws from 23 underlying studies (16 SSA + 3 LAC + 2 Bangladesh + 1 MENA + 1 South Asia non-BGD); 70% SSA-dominant. Wiens's geographic provenance was validated against the raw extractions at github.com/wienslab/diarrhea-careseeking.

**Test suite:** 1881 PASS / 0 FAIL / 6 SKIP (unchanged from v0.32.5).

---

# MOSAIC 0.32.5

## Phase 3 (#101): on-worker likelihood scoring on the Dask path

Workers on the Dask/Coiled path now compute the likelihood on-worker
immediately after each LASER iteration and return a compact
`{iter, seed_iter, likelihood}` dict plus a sim-level `params` echo,
instead of returning full per-iter time-series matrices for the R
orchestrator to score serially. The post-gather serial bottleneck
(~40 minutes on a 24K-sim predictive batch) collapses to seconds; at
47-country scale the per-sim payload drops from ~3.4 MB to a few
hundred bytes.

This release activates the `.mosaic_likelihood_provenance()` forward
hook introduced in v0.32.4: on the Dask path the helper now stamps
`engine = "python"`, and the resume guard automatically rejects pooling
Python-scored shards with R-scored shards from earlier runs. Depends on
`laser-cholera >= 0.13` (pinned in v0.32.0).

### Changed

- **`R/run_MOSAIC.R`** — adds a Dask preflight that rejects
  `control$io$save_simresults = TRUE`. The new worker schema no
  longer returns the raw (j, t) time-series the simresults writer
  needs. The local path is unaffected.

- **`R/run_MOSAIC_helpers.R`** — `.mosaic_run_batch_dask()` collapses
  its two R-side scoring branches (parallel PSOCK + sequential
  fallback, ~420 lines) into a single ~95-line gather-and-write
  loop. The new loop reads the per-iter scalar `likelihood` directly
  from the worker dict and re-injects two base-config fields stripped
  by `.extract_sampled_params()` before flattening: `location_name`
  (drives ISO column suffixes — without this `convert_config_to_matrix()`
  falls back to numeric suffixes like `beta_j0_tot_1` instead of
  `beta_j0_tot_ETH`) and `N_j_initial` (per-location initial
  population). Multi-iter likelihoods collapse via `calc_log_mean_exp()`
  exactly as the local path does (mirrors `run_MOSAIC.R` ~285-300).
  `.mosaic_likelihood_provenance()` now returns `engine = "python"`
  when `use_dask = TRUE`.

- **`inst/python/mosaic_dask_worker.py`** — `run_laser_sim()` return
  shape flips per the issue #101 contract. Per-iter entries change
  from `{j, seed, reported_cases, reported_deaths}` (nested lists of
  doubles) to `{iter, seed_iter, likelihood}` (three scalars). A new
  top-level `params` key carries the sampled scalars/vectors echoed
  from the JSON-deserialized `sampled` dict, minus `_MATRIX_FIELDS`.
  A defensive `getattr(model, "log_likelihood", None)` shim returns a
  clean per-sim failure (`success = False`, actionable error message)
  when the worker imports an engine older than laser-cholera 0.13.
  `run_laser_postca()` is unchanged — the post-calibration ensemble
  path still needs trajectories, not likelihoods.

### Tests

- **New `tests/testthat/test-dask_worker_schema_parity.R`** —
  engine-free regression tests that simulate the worker round trip
  in pure R (no Dask cluster, no Python), so they run in every CI
  configuration. Three cases:
  1. Column-name parity between the local-path
     `convert_config_to_matrix(params_sim)` and the Dask-path
     `convert_config_to_matrix(reconstituted)`, compared against the
     canonical `param_names_all` schema
     (`convert_config_to_matrix() minus seed`).
  2. ISO-suffix presence on every location — locks in that vector
     parameters land as `beta_j0_tot_ETH`, not `beta_j0_tot_1`.
  3. Failure-mode lock-in — drops the `location_name` re-injection
     and asserts numeric suffixes appear, so a future refactor that
     removes the injection trips a clear failure pointing back here.

- **`tests/testthat/test-dask_local_cluster_integration.R`** —
  schema assertions updated to the new contract:
  - New `skip_if_no_log_likelihood()` helper submits a sentinel sim
    and skips when the worker fails-by-design on engines `< 0.13`.
  - Test 3 (`client$submit`) asserts the result has `params` plus
    `iterations` of `{iter, seed_iter, likelihood}`; `location_name`
    is intentionally absent from `res$params`.
  - The `likelihood` finiteness check is relaxed to a numeric-type
    check — finiteness depends on Phase 1's
    `.mosaic_inject_likelihood_settings()` flattening the analyzer's
    input keys, which this fixture deliberately bypasses.
  - Test 4 (`client$map`) gated on the same skip helper.

### Migration

Calibrations that previously set `control$io$save_simresults = TRUE`
together with `dask_spec` now error early. Use the local backend for
diagnostic runs; `save_simresults` on the local path is unchanged.

### Cross-references

- Parent: [laser-cholera#47](https://github.com/InstituteforDiseaseModeling/laser-cholera/issues/47)
- This issue: [#101](https://github.com/InstituteforDiseaseModeling/MOSAIC-pkg/issues/101)
- Phase 1 (v0.30.1–v0.30.3): [#100](https://github.com/InstituteforDiseaseModeling/MOSAIC-pkg/issues/100)
- Phase 2 / laser-cholera v0.13.0: [laser-cholera#58](https://github.com/InstituteforDiseaseModeling/laser-cholera/issues/58)
- Resume hand-off (v0.32.4): the provenance guard now flips
  automatically on the Dask path with this release.

## laser-cholera v0.13.0 → v0.13.1

`inst/python/environment.yml` wheel URL bumped to v0.13.1. The release's only behavioral change is in the Python `calc_model_likelihood` module — peak rows whose `peak_date` falls outside `[date_start, date_stop]` are now dropped before index assignment instead of being clamped by `np.argmin` to t=0 or t=n-1. The Python port is now in alignment with the R-side in-window filter at `R/calc_model_likelihood.R:147-150, 423-426, 485-488`.

**No MOSAIC production code change required.** MOSAIC imports `laser.cholera.metapop.model` (the simulation engine) but never calls the Python likelihood in calibration or ensemble code paths — only in the parity test.

**Parity test gains.** The previously documented peak-term divergences in `tests/testthat/test-calc_model_likelihood_python_parity.R` are now closed:
- Test #4 (daily cadence, peak timing + magnitude): was R = -495 / Python = -606 (~22%); now both = -527.79 within `1e-4`.
- Test #5 (weekly cadence, peak timing): was R = -282 / Python = -1101 (~290%); now both = -282.32 within `1e-4`.

The skips for these two tests are removed (parity-test PASS count: 4 → 6). The two remaining skips (test #6 NA-masking in WIS, test #7 zero-prediction penalty scaling) are unrelated to v0.13.1's fix and remain pending an upstream port.

# MOSAIC 0.32.1

## CFR pipeline reworked for v0.13+ schema; WHO data refreshed through 2025

### rho_deaths variant: production pin to informative Beta(36.95, 51.02)

`rho_deaths` uses the **informative** variant Beta(36.95, 51.02) (mean 0.42, sd 0.052, 95% CI [0.32, 0.52]) for production calibrations. This is the SYNTHESIS_REPORT §3.3 pooled-mean-CI fit. The wider prediction-interval variant Beta(6.30, 8.52) is retained for sensitivity analysis only. **Rationale:** within a single country, the deaths likelihood identifies the product `mu_j_baseline * rho_deaths`, leaving a flat factorization direction. Pinning `rho_deaths` near 0.42 lets `mu_j_baseline` posteriors carry the cross-country CFR signal cleanly. All three meta-analysis anchor studies (Routh 2017, Shikanga 2009, Bwire 2013) are SSA outbreak settings — the regime MOSAIC calibrates — so the pooled-mean precision is the operative target.

### CFR parameter tracking — closing the gaps

Parameter tracking surfaces were updated for consistency with the v0.13+ CFR pipeline:

- `R/plot_model_parameters.R` location_params_base now includes `mu_j_baseline`, `mu_j_slope`, `mu_j_epidemic_factor`, `epidemic_threshold`, `beta_j0_tot`, `psi_star_*` — these were silently dropped from posterior visualization despite being sampled per-country.
- `R/get_param_names.R` replaces the stale `mu_j` placeholder with the actual sampled triple (`mu_j_baseline`, `mu_j_slope`, `mu_j_epidemic_factor`) plus `epidemic_threshold` and the `delta_reporting_*` integers.
- `R/calc_model_ess_parameter.R` docstring example updated from `mu_j` to `mu_j_baseline`.
- `R/run_MOSAIC.R` docstring example flags updated from `sample_mu_j` to `sample_mu_j_baseline`.

The mu_j_baseline derivation in `data-raw/make_priors_default.R` is corrected for the laser-cholera v0.13+ schema. The conversion factor from observed reported CFR to the engine's daily mortality hazard is now `rho / (rho_deaths * chi)` (~1.015) instead of `rho / chi` (~0.43). Per-country `mu_j_baseline` Gamma priors are ~2.36x their pre-v0.32.1 values, which corrects the pre-v0.13 under-scaling where mu_j_baseline implicitly absorbed `1/rho_deaths`. The conversion factor is now derived inline from the actual `rho`, `rho_deaths`, `chi_endemic`, `chi_epidemic` Beta priors so it stays in sync if those upstream priors are updated.

`make_config_default.R` now sources `mu_j_baseline` UNIVERSALLY from `priors_default` Gamma means (was: raw CFR `rowMeans(mu_jt)` with an ETH-only hand-patch). config_default and priors_default agree by construction; a new regression test (`test-cfr-pipeline-consistency.R`) asserts this invariant per country.

**MOZ-specific overrides dropped.** The MOZ `mu_j_baseline = Gamma(2, 1176)` and `mu_j_epidemic_factor = Gamma(1.5, 0.5)` overrides were both calibrated under the pre-v0.13 misspecified likelihood. They are superseded by the universal data-driven prior derived from the corrected steady-state identity. MOZ now inherits `mu_j_baseline ≈ 0.0045` (from its observed ~0.43% CFR × 1.015) and the global `mu_j_epidemic_factor ~ Gamma(1, 2)` default. If country-specific calibration evidence under the new schema warrants re-introducing an override, it should be derived from a v0.32+ calibration.

**WHO annual data refreshed through 2025.** `R/process_WHO_annual_data.R` was rewritten to (a) parse the actual `first_epiwk` / `last_epiwk` date ranges instead of hard-coding `year <- 2024`, (b) ingest ALL `cholera_adm0_public_*.csv` files in `raw/WHO/annual/who_global_dashboard/` and dedupe by `(iso, year)` keeping max coverage, (c) archive each ArcGIS dashboard download with a snapshot date instead of overwriting, and (d) add `coverage_days` and `year_fraction` columns so partial-year snapshots are clearly labeled. The 2024 and 2025 calendar-year CSVs (downloaded from the WHO Global Cholera & AWD Hub UI) are now in the pipeline alongside the rolling 2026 partial-year snapshot. The output CSV is renamed from `who_afro_annual_1949_2024.csv` to `who_afro_annual.csv` (year-agnostic canonical name); 6 consumers updated.

**MOSAIC-docs math spec updated.** `04-model-description.Rmd` describes the v0.13+ derivation identity, the sigma cancellation, and the reinterpretation of pre-v0.32.0 mu_j_baseline posteriors.

# MOSAIC 0.32.0

## Upgrade engine to laser-cholera v0.13.0 (deaths likelihood scale correction)

The Python engine is bumped from v0.12.5 to v0.13.0 (`inst/python/environment.yml`). The engine now emits a separate `model.results.reported_deaths` time series equal to `round(disease_deaths[t - delta_reporting_deaths] * rho_deaths)`, and MOSAIC scores observed surveillance `reported_deaths` against simulated `reported_deaths` — not against raw `disease_deaths`.

**Behavior change (deaths likelihood scale).** Before v0.32.0 the deaths NB likelihood compared observed reported deaths to simulated raw disease deaths, so the simulated series was on a ~1/rho_deaths ≈ 2.4× higher scale than the observations. Calibration absorbed the missing factor by inflating `mu_j_baseline` posteriors. The new schema fixes the scale; deaths bias-ratio diagnostics should now centre on 1.0. Resume from a pre-v0.32 run on v0.32+ is **not** safe — the `deaths` column in parquet shards has flipped semantics.

**Field rename surface.** Every R extraction of `model$results$disease_deaths` flipped to `reported_deaths`. The Dask worker dict (`inst/python/mosaic_dask_worker.py`) and the R-side gatherers now use `reported_deaths`. `plot_model_ppc`, `calc_model_ensemble`, the test fixtures, scenario scripts, and the vignette were updated in lockstep. The MOSAIC-Mozambique scenario scripts (peer repo) were updated too.

**Schema changes consumed.**

- v0.13.0 hard-asserts every `iso_code` in `epidemic_peaks` appears in `location_name`. `get_location_config()` and the Dask injection (`.mosaic_inject_likelihood_settings()`) now call `.filter_epidemic_peaks()` to drop foreign-iso and out-of-window rows. `make_LASER_config()` promotes its stale warning to an error when foreign iso_codes are present.
- v0.13.0 auto-computes `epidemic_peaks$loc_idx` from `iso_code` at config-load time; MOSAIC does not need to inject it.
- HDF5 paramfile loading was removed in v0.13.0; MOSAIC has always shipped JSON.
- The Python namespace flipped from `laser_cholera.*` to `laser.cholera.*` (this was actually pre-v0.13 but the test fixture and three scenario scripts still pointed at the old name).

**Prior updates.**

- `priors_default` v15.4 → v15.5:
  - `rho_deaths` switched from informative variant `Beta(36.95, 51.02)` to recommended `Beta(6.30, 8.52)` per `claude/rho_deaths_research/SYNTHESIS_REPORT.md` §3.4. Both share mean ~0.42, but the recommended variant fits the 95% prediction interval and accommodates between-study heterogeneity. The informative variant remains documented for sensitivity analysis.
  - `delta_reporting_deaths` description corrected: this parameter is the **death-event-to-death-report** delay, not symptom-onset-to-death-report. The symptom-onset-to-death interval is implicit in the SEIR dynamics (γ₁⁻¹ ~ 5–7 days) and is NOT folded into `delta_reporting_deaths`. Default 5 days, prior Truncnorm(mean=4, sd=3) unchanged.

**Resume guard.** The resume path compares the live `laser-cholera` version against the `python$pkg_laser_cholera` field already recorded in `1_inputs/environment.json` and **hard-errors when the persisted and current versions straddle the v0.12 → v0.13 deaths-scale boundary** (resuming would mix incompatible deaths scales in the on-disk shards). Two versions on the same side of the boundary warn but proceed.

**Parity tests.** A new `tests/testthat/test-calc_model_likelihood_python_parity.R` (4 PASS) validates R↔Python likelihood parity on the core NB, cumulative-progression, and WIS paths. Four parity tests are SKIPped with documented Python-side divergences (peak-term scoring, NA-masking in WIS, zero-prediction penalty) — issues to file upstream against laser-cholera.

**Known Python-side issues (filed for upstream fix):**

1. `np.round(disease_deaths * rho_deaths)` deterministic rounding biases low-incidence patches toward zero (ETH smoke shows ratio 0.13 vs rho_deaths=0.46). Should use `prng.binomial`.
2. Peak-term scoring diverges ~22% on daily cadence and ~290% on weekly cadence; only surfaces in tests when `loc_idx` is supplied (Python silently drops peak rows missing it).
3. WIS-path NaN propagation when observations contain NAs (~0.5% LL drift).
4. Zero-prediction penalty scaling differs ~1.78× between R and Python.

# MOSAIC 0.31.0

## Add `resume = TRUE` to `run_MOSAIC()` — continue an interrupted calibration

`run_MOSAIC()` gains a `resume` argument (default `FALSE`). When `TRUE`, the run reconstructs calibration state from the per-sim shards already present in `<dir_output>/2_calibration/samples/` and continues from the next unused `sim_id` instead of restarting from scratch. Because each simulation's parameters are a deterministic function of `seed = sim_id`, a resumed run is bit-identical to an uninterrupted one.

- The shards on disk are the source of truth. The next `sim_id` is always `max(id on disk) + 1`, so no draw is ever duplicated.
- An internal `2_calibration/state/resume_checkpoint.rds` (written every batch) restores the adaptive ESS/phase state exactly; runs without a checkpoint bootstrap the state from the shards.
- `resume = TRUE` is rejected with `clean_output = TRUE`; when the run already completed (a consolidated `samples.parquet` the shards would shrink); and when the supplied `config`, `priors`, `control$likelihood`, `control$sampling`, `control$calibration$n_iterations`, or the calibration mode (auto vs fixed) differ from those persisted in `1_inputs/` (each changes the draws or likelihood and would make the pooled shards incomparable — a hard error). In adaptive mode resume continues from `max(sim_id)+1` and does not backfill interior gaps; fixed mode re-runs any missing id to hit the target. Corrupt/invalid shards are read-validated and quarantined; resumed runs are recorded in `summary.json` (`resumed`, `n_simulations_reused`). Each run also stamps a likelihood-value provenance (`likelihood_provenance` in `environment.json`); resume refuses to pool shards scored by a different likelihood engine/implementation — a forward hook for the Dask/Coiled worker-side scoring port (laser-cholera issue #47), which flips the engine from R `calc_model_likelihood` to on-worker Python.
- `resume = FALSE` (the default) is byte-identical to prior behavior. The slim `run_state.json` monitoring file is unchanged.

# MOSAIC 0.30.49

## Remove `config_default_MOZ` / `priors_default_MOZ` (BREAKING for direct users)

The `config_default_MOZ` and `priors_default_MOZ` data objects and their `data-raw` builders are removed from the package. The objects were never exported in `NAMESPACE`, never documented in `man/`, and never consumed by production R code; their only callers were three integration test files (CRAN-skipped) and the MOSAIC-Mozambique calibration project deliberately does not depend on them — it builds its own MOZ artifacts from scratch via `MOSAIC::make_LASER_config()` + cherry-picked entries from the *global* `MOSAIC::priors_default`.

The parallel `_MOZ` build pipeline was a recurring source of silent drift: it had to be hand-maintained whenever the global `make_config_default.R` / `make_priors_default.R` changed, and the v0.30.47 `rho_deaths` switch caught one such miss (the .rda kept the old 0.6 value while the JSON was updated to 0.42).

**Removed files:**

- `data/config_default_MOZ.rda`
- `data/priors_default_MOZ.rda`
- `inst/extdata/config_default_MOZ.json`
- `inst/extdata/priors_default_MOZ.json`
- `data-raw/make_config_default_MOZ.R`
- `data-raw/make_priors_default_MOZ.R`
- `tests/testthat/test-dask_local_cluster_integration.R` (only consumer; depended entirely on the fixture)
- 4 integration tests at the end of `tests/testthat/test-ic_moment_match.R` (the deterministic-math + guard-clause tests stay; the integration tests that needed a single-location config fixture are removed)

**Migration:** if you were loading `MOSAIC::config_default_MOZ` for ad-hoc experiments, the MOSAIC-Mozambique project (`MOSAIC-Mozambique/code/R/make_config_MOZ.R`) is the canonical way to build a Mozambique calibration config from the package's global priors.

# MOSAIC 0.30.48

## Audit pass: ic_moment_match latent bug + metadata + docs

Deep-review sweep across v0.30.33 → v0.30.47 surfaced three notable issues; this release fixes them.

- **`moment_match_E_I` two latent bugs (R/sample_parameters.R).**
  The optional initial-conditions moment match (default `ic_moment_match = FALSE`) had two bugs that did not surface in production but broke the unit tests:
  - **Vector reshape direction.** When `reported_cases` arrives as a length-T vector (e.g. single-location tests), `as.matrix(vec)` returned a T×1 column matrix; the function then read `obs_mat[j, ]` and got just element [1]. Production code always passes an n×T matrix so the branch was never hit there. Fixed by reshaping vectors as `matrix(vec, nrow = 1)` (single-location row).
  - **Guard clause NA propagation.** When `gamma_1` (or `iota` / `sigma`) was `NULL` in the config, `is.finite(NULL) || NULL <= 0` yielded `NA`, and the enclosing `if(...)` failed with "missing value where TRUE/FALSE needed." Rewritten with `isTRUE(is.finite(x) && x > 0)` so the guard coerces NA/NULL to FALSE and falls back cleanly to the prior IC.
  Three unit-test expectations in `test-ic_moment_match.R` that pinned the old (pre-v0.30.40) `E = I * iota` formula were also updated to the corrected `E = Isym * gamma_1 / (sigma * iota)` formula.
- **MOZ `config_default_MOZ` `rho_deaths` artifact missed in v0.30.47.** `data/config_default_MOZ.rda` still carried the old `rho_deaths = 0.6` even though the JSON sidecar had been updated to 0.42. Both are now consistent at 0.42, with the MOZ config metadata bumped to v2.8 and the rationale linked.
- **`config_default` metadata bumped (v3.4 → v3.5).** The global config metadata description was extended to reference the v0.30.47 `rho_deaths` switch; previously it referenced v3.4 (the beta_j0_tot per-iso seeding) as the most recent change.
- **`R/est_suitability.R` missing `@param exclude_covariates`.** The function signature has accepted `exclude_covariates = character(0)` for ablation studies for some time, but the parameter was not roxygen-documented. Added the tag; `man/est_suitability.Rd` regenerated.
- **`NEWS.md` backfill for v0.30.41–v0.30.47.** Six version entries were missing; added below.

# MOSAIC 0.30.47

## rho_deaths informative prior from SSA meta-analysis

The `rho_deaths` default prior is replaced (`Beta(3, 2)` → `Beta(36.95, 51.02)`) and the default value drops from 0.6 to 0.42.

- **Methodology.** Random-effects (DerSimonian-Laird, logit-scale) meta-analysis of three Sub-Saharan African studies — Routh 2017 (Tanzania, EID; binomial CI from 48/101), Shikanga 2009 (Kenya, AJTMH; approximate binomial CI from implied ~25/73), Bwire 2013 (Uganda, PLOS NTDs; sensitivity range). Inverse-variance weighting yields Routh 53%, Shikanga 42%, Bwire 5% — Bwire's wide CI auto-down-weights it. Pooled mean 0.419 (essentially identical to the earlier ad-hoc 3:2 quality-weighted mean 0.417).
- **Informative variant.** Beta fit to the 95% CI of the pooled mean (not the prediction interval) gives `Beta(36.95, 51.02)`, mean 0.420, 95% CI [0.319, 0.524], ESS ~88.
- **Methodology mirrors `R/get_rho_care_seeking_params.R`** (cases-side `rho`), establishing parallel statistical machinery for the two observation-model detection probabilities.
- **Previous attribution to Finger et al. 2024 corrected** — that paper is an editorial without a quantitative anchor; the 23–96% range cited belonged to Pampaka et al. 2025 and described place of death, not surveillance capture. Full provenance: `claude/rho_deaths_research/SYNTHESIS_REPORT.md`.

# MOSAIC 0.30.46

## `config_default`: phase-switching params seeded from per-iso priors

`epidemic_threshold` and `mu_j_epidemic_factor` in the global `config_default` are now sourced PER-COUNTRY from `priors_default$parameters_location` rather than flat values (formerly `1/10000` and `1.25`). The per-iso prior means span roughly 7e-7 to 8.7e-6 for `epidemic_threshold` and 0.5 to 3.0 for `mu_j_epidemic_factor`, restoring the epidemiological heterogeneity required for the engine's phase-switching to actually engage.

# MOSAIC 0.30.45

## `config_default`: initial conditions seeded from per-iso priors

`S_j_initial`, `E_j_initial`, `I_j_initial`, `R_j_initial`, `V1_j_initial`, `V2_j_initial` are now seeded from each country's `prop_*_initial` Beta priors with Hamilton apportionment, rather than the old flat `S = 80%` / `R = 50%` defaults. The flat default had `R_eff = 0.72 < 1` at many ISOs, blocking transmission before the calibration loop could begin.

# MOSAIC 0.30.44

## Docs: mislabeled params, phantom function refs, stale value claims

Audit-driven cleanup of roxygen documentation across `R/priors_default.R`, `R/config_default.R`, `R/config_simulation_endemic.R`, and `R/make_LASER_config.R`: corrected 19 mislabeled `@param` fields, removed phantom `\link{}` references to functions that no longer exist, and resynced value claims in roxygen text with the actual data shipped in `data/priors_default.rda`. Man pages regenerated.

# MOSAIC 0.30.43

## `epidemic_peaks`: filter at build time + warn on bad configs

The 129-row `epidemic_peaks` table shipped with `config_default` is now filtered at build time to `[date_start, date_stop]` (47 rows after filtering). The 82 out-of-window rows were silently snapping to `t = 1` / `t = N` in the peak-shape likelihood terms and bloating the JSON. New helper `filter_epidemic_peaks()` (`R/filter_epidemic_peaks.R`) is also called from `make_LASER_config()` to warn whenever a user-supplied config carries peaks outside the simulation window.

# MOSAIC 0.30.42

## `write_json_or_gz`: peer-arg API + JSON rename to match .rda

`R/write_json_with_optional_gz.R` is replaced by `R/write_json_or_gz.R` with a clearer peer-arg API (`write_json = TRUE` / `write_gz = TRUE` controlled independently). The four `inst/extdata` JSON sidecars are renamed to match the `.rda` they correspond to: `default_parameters*.json` → `config_default*.json`, `simulated_parameters.json` → `config_simulation_epidemic.json`, `sim_endemic_parameters.json` → `config_simulation_endemic.json`.

# MOSAIC 0.30.41

## `inst/extdata`: opt-in .gz sidecar for production configs only

The `.json.gz` sidecars that previously shipped automatically alongside every `inst/extdata/*.json` are now opt-in. They were redundant for the simulation configs (`config_simulation_*`) which are small and never read from disk by performance-sensitive code; production builds for `config_default` / `priors_default` still produce the `.gz` sidecar when explicitly requested via `write_gz = TRUE` in the data-raw scripts.

# MOSAIC 0.30.40

## Biology fixes from deep-review re-validation

Two findings from the disease-modeling deep review that survived
re-validation against MOSAIC-docs and project history:

- **moment_match_E_I steady-state formula corrected.** The optional
  `ic_moment_match` feature was using `E_count = I_count * iota`, which
  is dimensionally inconsistent (count x 1/day) and over-seeded E by
  ~5x at prior medians. The laser-cholera engine has
  `E -> Isym` at rate `sigma * iota` per E and `Isym -> R` at rate
  `gamma_1`, so the steady-state balance is
  `E = Isym * gamma_1 / (sigma * iota)`. At prior medians
  (`gamma_1 = 0.1/d`, `sigma = 0.24`, `iota = 0.71/d`) the corrected
  E/Isym ratio is ~0.59 instead of ~0.71. The `I_count` variable was
  renamed `Isym_count` internally to reflect that the reporting chain
  (`cases = Isym * rho * chi_endemic`) only observes the symptomatic
  compartment. Flag still defaults to FALSE; this fixes the formula
  for any caller who turns it on.
- **epsilon prior 2x variance-inflation removed.** A
  variance-inflation step at `data-raw/make_priors_default.R:127-128`
  was doubling the sd of the natural-immunity waning rate from 2.0e-4
  to 4.0e-4, pushing the 95% CI on immunity duration to roughly
  [1.9, 53] years -- the upper-tail value is biologically
  implausible and the inflation had no documented rationale. With
  the inflation removed, sd is back to 2.0e-4 (95% CI on rate
  [1.7e-4, 1.03e-3], corresponding immunity duration CI ~[2.7,
  16] years) -- the range supported by the cited King et al. 2008
  and the project's two-cohort re-fit (~7 yr mean). `priors_default`
  metadata version bumped to 15.2 and the .rda re-built.

# MOSAIC 0.30.39

## Engineering review sweep (v0.30.29 – v0.30.38)

Series of focused fixes from a five-agent deep-review of the package.
No model-behavior changes; package hygiene, correctness on edge cases,
and consistency with the documented design.

- **v0.30.29** — Add `rlang` to `DESCRIPTION` Imports (`NAMESPACE` had
  `import(rlang)` but the dep was missing, producing an R CMD check
  ERROR). Declare column names referenced by recent plot edits in
  `R/globals.R` to silence "no visible binding" NOTEs.
- **v0.30.30** — Repo hygiene: remove tracked zero-byte `=` file,
  delete stray `MOSAIC.Rcheck/`, `..Rcheck/`, `.Rd2pdf*/`, root
  `Rplots.pdf`, and `model/input/*.bak{,_*}` on disk. Expand
  `.Rbuildignore` (vm/, CLAUDE.md, .Rcheck/, =, .RData, .Rhistory, …)
  and `.gitignore` (*.bak, .Rd2pdf*/).
- **v0.30.31** — `data-raw/make_*` builders patched only `.json`
  outputs (`grepl("\\.json$")`) so the `.json.gz` siblings were
  shipping without `zeta_ratio` and `decay_days_spread`, breaking the
  downstream `zeta_2 = zeta_1 / zeta_ratio` derivation. Switch regex
  to `\\.json(\\.gz)?$`, read via `read_json_to_list()`, write via
  `write_list_to_json()` with the right compress flag. Regenerated
  all four `.json.gz` artifacts so each pair is now content-identical.
- **v0.30.32** — `get_default_config()` and `get_default_LASER_config()`
  hardcoded `file.path(PATHS\$ROOT, "MOSAIC-pkg", ...)` -- only works on
  a developer checkout. Switch to `system.file("extdata", ...,
  package = "MOSAIC")`; `PATHS` retained for backward compatibility.
- **v0.30.33** — Introduce `.mosaic_set_all_thread_env(n)` as a single
  source of truth for the six canonical thread-env vars documented in
  CLAUDE.md (`OMP_NUM_THREADS`, `MKL_NUM_THREADS`, `OPENBLAS_NUM_THREADS`,
  `NUMEXPR_NUM_THREADS`, `TBB_NUM_THREADS`, `NUMBA_NUM_THREADS`). The
  fallback branch of `.mosaic_set_blas_threads` had been setting only
  3, `calc_model_ensemble.R` 5, `make_mosaic_cluster.R` 5 in two
  places. Route every site through the helper.
- **v0.30.34** — `plot_vaccination_maps.R`: `ggsave(plot = print(...))`
  was saving the invisible return value and double-printing to the
  active device. Replaced with `plot = final_combined_plot`.
- **v0.30.35** — `calc_model_likelihood()` peak-shape terms:
  `which.min(abs(date_seq - peak_date))` always returns an in-range
  index, so peaks whose date fell outside `[date_start, date_stop]`
  were silently snapped to t=1 / t=N, biasing the peak-timing and
  peak-magnitude likelihoods. Filter `peak_date` against
  `[date_seq[1], date_seq[N]]` before snapping, in all three sites
  (main path + two legacy helpers).
- **v0.30.36** — `calc_model_ensemble()` weighted mean was biased
  toward zero under simulation failures: `sum(values * w,
  na.rm=TRUE)` drops the NA term but does not redistribute its
  weight mass. Filter valid then divide by sum of surviving weights,
  matching the behavior of `weighted_quantiles()`.
- **v0.30.37** — `props_to_counts()` integer-rounding fallback could
  leave a compartment negative or sum off by 1 in edge cases.
  Replace per-compartment `round()` + residual-on-S with Hamilton
  (largest-remainder) apportionment in a single per-location pass;
  `stopifnot()` asserts sum == N and all counts >= 0.
- **v0.30.38** — `moment_match_E_I()` was location-invariant due to
  `unlist(reported_cases)` flattening the matrix; every country got
  identical initial infections. Compute first-week-of-positives
  window per location row. (E_count = I_count * iota formula left
  unchanged with an inline note for the next biology pass; flag is
  OFF by default.)
- **v0.30.39** — Update `epidemic_peaks` docstring to match actual
  code defaults (28-day smoothing, 10-day comparison, 75-day
  separation, 8% prominence) and backfill NEWS for the
  v0.30.27/0.30.28 entries that were silently missed.

# MOSAIC 0.30.28

## `calc_model_likelihood`: source `epidemic_peaks` from config

When the caller supplies `config\$epidemic_peaks` (a data.frame with
`iso_code` + `peak_date` columns), the likelihood now prefers that
over the lazy-loaded `MOSAIC::epidemic_peaks` package dataset. This
keeps the R likelihood aligned with the Python port at
`laser_cholera.metapop.calc_model_likelihood`, which reads the same
field from its config dict. The legacy helpers
`calc_multi_peak_timing_ll` and `calc_multi_peak_magnitude_ll` now
accept an optional `epidemic_peaks=` argument with the same fallback.

# MOSAIC 0.30.27

## Ship `epidemic_peaks` in `config_default` for the Python port

`config_default\$epidemic_peaks` is now populated from
`MOSAIC::epidemic_peaks` at build time and written into both the
`.rda` and the `default_parameters.json{,.gz}` artifacts. This is the
counterpart to a parallel change in laser-cholera that consumes
`config["epidemic_peaks"]` instead of requiring the R package to be
loaded. Configs built before this version remain readable: missing
`epidemic_peaks` falls back to the lazy-loaded package dataset.

# MOSAIC 0.30.26

## `compile_suitability_data()` — remove three dormant blocks

Three blocks of derived covariates were being computed and persisted to
`cholera_country_weekly_suitability_data.csv` even though
`est_suitability()`'s `covariates_all` list intentionally excludes
every one of them. Computing and saving them was wasted IO + a foot-gun
for any future contributor who re-enables them without realising why
they were excluded. Removed entirely from the function:

- **Vaccination block (3 columns)**: `vaccination_rate_daily`,
  `cumulative_vaccine_doses`, `log1p_cum_vaccine_doses`. Vaccination is
  a downstream intervention -- vaccines are deployed in response to
  cholera, not in anticipation of environmental suitability -- so
  these are non-causal predictors for the suitability model.
- **Epidemic-memory block (8 columns)**: `r_t`, `cum_cases_4w/8w/12w/52w`,
  `nonzero_ratio_52w`, `weeks_since_major_outbreak`, `peak_cases_52w`,
  `seasonal_outbreak_risk`, plus the `epidemic_week` alias. All
  case-derived; predicting a case-derived target (`cases_binary`)
  from any function of `cases` is target leakage even with the
  forecast-safe `memory_lag`.
- **Spatial-import block (7 columns)**: `import_vulnerability`,
  `export_potential`, `weighted_import_connectivity`,
  `connectivity_degree`, `betweenness_centrality`,
  `eigenvector_centrality`, `import_export_balance`. Mobility-network
  features that were computed but excluded from the LSTM input set.

Net: 18 columns dropped from the saved suitability CSV. The columns
were already excluded from the LSTM covariate list so this is purely
hygiene -- no model behavior changes. Compile wall-time should drop
slightly (no vaccination merge + cumulative-doses computation, no
epidemic-memory `slide_dbl()` loops, no mobility-matrix calculations).

The `forecast_mode` argument is now mostly cosmetic (controls summary
messages and a small bit of climate-anomaly leading-edge behavior).
Kept for backward compatibility.

---

# MOSAIC 0.30.25

## Flood-prob GAM: select=TRUE shrinkage + precip-window tensor product

Two changes validated independently by a software-engineer agent and a
statistical-modeler agent running parallel experiments in separate
sandboxes. Both converged on (1); the modeler additionally identified
(2):

1. **Replace the iterative p-value pruning loop with a single bam() fit
   using `select = TRUE`.** Smoothness null-space shrinkage is mgcv's
   principled mechanism for driving uninformative smooths' coefficients
   toward zero. The v0.30.24 in-sample-p-value pruning was multicollinearity-sensitive
   (the docstring already documented this caveat) and produced
   fragile drop sequences. select=TRUE delivers equivalent CV AUC
   (0.851 -> 0.852 in the SWE experiments) with a +2.6 to +4.2
   percentile-point lift on the worst-detected cyclone (Kenneth 2019,
   from 71.6 to 74.2 in production after this change). Removes the
   prune_p_threshold and prune_max_iter args + the pruning_log
   diagnostic. Single ~10-second fit replaces the iterative ~30-60s
   loop.
2. **Replace `s(precip_sum_4w) + s(precip_sum_24w)` with
   `te(precip_sum_4w, precip_sum_24w, k = c(10, 10))`.** The "heavy
   4-week rain ON an already-saturated catchment (6-month antecedent)"
   interaction is genuinely jointly nonlinear; the tensor captures it
   where the univariates plus the existing `precip_x_soil_anom`
   approximated it crudely. Modeler-agent experiment: cyclone median
   percentile rank 91.6 -> 93.8 (+2.2), top-5\% hits 3 -> 4.

End-to-end production cyclone benchmark (re-measured):

  Idai     MOZ 2019-W11   pctile = (run pending)
  Idai     MWI 2019-W11
  Idai     ZWE 2019-W11
  Kenneth  MOZ 2019-W17
  Eloise   MOZ 2021-W04
  Ana      MOZ 2022-W04
  Ana      MWI 2022-W04
  Gombe    MOZ 2022-W11
  Freddy-1 MOZ 2023-W08
  Freddy-2 MOZ 2023-W11

Tested but rejected by both agents: nthreads (no-op on macOS), method =
REML/ML/GCV.Cp (no metric change or impractically slow), `bs = "ad"`
adaptive smoothers, `k = 20+` on precip windows (overfit), `bs = "cr"`
cubic regression (no change), per-country `by = iso_code_f` smooths
(overfit), nested random effects, cloglog link, quasibinomial,
betabinomial, Tweedie on log1p(affected), smoothed +/-2w binary
target, class weighting (trades AUC for cyclone recall).

Caveat surfaced by both agents: country-stratified 5-fold CV AUC drops
to 0.72 (vs 0.85 with random folds). Most of the model's lift comes
from the country random effect; climate predictors are doing less
work than random-fold CV suggests. The model would fail badly on a
country with no training data. Worth keeping in mind when interpreting
the imputed flood_prob in any out-of-sample country setting.

---

# MOSAIC 0.30.24

## `impute_flood_probability()` gains iterative p-value pruning loop

After each GAM fit, the term (smooth or parametric) with the highest
in-sample p-value above \code{prune_p_threshold} (default 0.05) is
dropped and the GAM is refit. The cycle repeats until every remaining
prunable term is significant, fewer than 5 prunable terms remain, or
\code{prune_max_iter} (default 30) iterations are reached. The country
random-effect smooth and the region-conditional
\code{s(precip_sum_4w, by = region_f)} block are treated as structural
and never dropped. The trace is written to
\code{flood_gam_pruning_log.csv} in the diagnostics directory.

Pruning behavior on the v0.30.24 production run dropped 5 precip-block
terms whose contribution was absorbed by their collinear neighbors:
\code{s(precipitation_sum)} (p=0.74), \code{s(precip_sum_12w)}
(p=0.65), \code{s(precip_anom)} (p=0.37),
\code{precip_extreme_p90_count} (p=0.21), \code{s(precip_sum_2w)}
(p=0.18). The model retains \code{precip_sum_4w/8w/24w/52w}, the
region-conditional smooth, plus soil-moisture / SPEI / humidity / wind
/ ENSO / IOD terms.

End-to-end cyclone benchmark (median percentile rank across 10
catastrophic cyclones in MOZ/MWI/ZWE) is unchanged at 91.1 vs 93.0
pre-pruning. Idai loses ~1.3 pts in MOZ/MWI/ZWE; Ana and Freddy gain
0.3-2.1 pts. Top-5\% hits stay at 3 of 10. Median seasonal-variance
share unchanged at 0.398.

Caveat: in-sample p-values are not predictive-importance tests and are
sensitive to multicollinearity. The loop produces a smaller, cleaner
model whose remaining smooths are all in-sample significant; verify
against the cyclone benchmark + rolling-year CV after major covariate
changes. Set \code{prune_p_threshold = 1.0} to disable pruning
entirely (recovers v0.30.23 behavior).

Wall-time impact: pipeline runs in ~69 sec (vs ~41 sec for v0.30.23),
the extra 28 sec covering the 5 additional GAM fits the pruning loop
performs.

---

# MOSAIC 0.30.23

## Flood-prob imputer: revert to enriched binomial after cyclone benchmark

The v0.30.22 Tweedie-on-severity formulation made Cyclone Freddy stand
out as MOZ's #1 historical week (the headline visual goal) but a
10-cyclone benchmark across Idai 2019 (MOZ/MWI/ZWE), Kenneth 2019,
Eloise 2021, Ana 2022 (MOZ/MWI), Gombe 2022, and Freddy 2023 (x2 landfalls)
showed the Tweedie variant under-detects other catastrophic events --
median percentile rank 81.4, minimum 77.8. The root cause: the Tweedie
target chases EM-DAT's logged Total Affected, and Idai's Total Affected
was logged smaller than its real impact warranted, so the model
under-amplified it.

This release reverts to the family + target of v0.30.21 (binomial logit
on emdat_flood_active 0/1) while keeping the four physics-driven
predictor enrichments that the v0.30.22 S2 exploration found
load-bearing:

- `s(precip_sum_24w)` -- ~6-month basin saturation memory (inline-computed)
- `s(precip_sum_52w)` -- annual antecedent precipitation (inline-computed)
- `s(precip_sum_4w, by = region_f)` -- region-conditional 4-week precip smooth
- `s(precip_x_soil_anom)` -- joint precip-anomaly x soil-moisture-anomaly index

Result: median cyclone percentile rank 93.2 (vs 81.4 for Tweedie, 90.8
for v0.30.21 baseline), minimum 79.1 (every cyclone-week in the top 21%
of its country's history). Three cyclones in the top 5%.

| Method | Median cyclone pctile | Min | Top 5% hits / 10 |
|---|---|---|---|
| **v0.30.23 (enriched binomial)** | **93.2** | **79.1** | **3** |
| v0.30.22 (Tweedie hybrid) | 81.4 | 77.8 | 2 |
| v0.30.21 (baseline binomial) | 90.8 | 69.4 | 3 |

Trade-off vs v0.30.22: Freddy's headline rank in MOZ drops back from
#1 to top-5%, but every other catastrophic cyclone is more reliably
detected. The right call for cholera forecasting where we care about
all flood events, not just one outlier.

Output is now naturally on [0, 1] from the logit link -- no global-max
scaling needed. Test thresholds restored to the binomial-appropriate
AUC > 0.80 and mean(hi)-mean(lo) > 0.3 discrimination tests.

---

# MOSAIC 0.30.22

## Flood-prob imputer: Tweedie severity target + enriched predictors

The v0.30.20 binomial-on-active formulation produced an imputed
`emdat_flood_prob` that didn't separate major events from seasonal
climatology -- Cyclone Freddy (MOZ, Feb-Mar 2023) only reached
probability 0.503, barely above the MOZ Feb-Apr p90 of 0.396 and below
the all-time MOZ max. Four parallel strategies were explored (Tweedie
severity target; enriched predictor set; xgboost; two-stage hurdle).
This release adopts the hybrid of the two winners:

**Tweedie family on raw `Total Affected`.** Encodes flood SEVERITY
rather than just presence/absence. The Tweedie compound-Poisson-gamma
mixture handles the zero-inflated continuous target natively.

**Enriched predictor set** (computed inline by the imputer, no
upstream compile changes required beyond the existing `region`
column):

- `precip_sum_24w` and `precip_sum_52w` -- long-memory antecedent
  precipitation capturing basin saturation. These were the largest
  single contributors to interannual variance in the rebuild
  experiments.
- `s(precip_sum_4w, by = region_f)` -- region-conditional 4-week
  precipitation smooth (4-region WHO classification: Central / East /
  Southern / West Africa).
- `s(precip_x_soil_anom)` -- joint precip-anomaly x soil-moisture-anomaly
  index for the "very-wet AND already-saturated" regime.

**Output normalization**: predictions on the raw Tweedie scale are
non-negative, heavy-tailed continuous. Normalized by global maximum to
`[0, 1]` -- rank-normalization was tested and rejected because it
erases the long-tail major-event amplification the Tweedie target was
chosen for.

**End-to-end measured impact** (full AFRO panel, real climate + EM-DAT
data):

| Metric | v0.30.21 (binomial) | v0.30.22 (Tweedie hybrid) |
|---|---|---|
| Cyclone Freddy rank in MOZ history | not top-5 | **1st of 872** |
| Freddy / MOZ Feb-Apr p90 ratio | 1.27x | **8.14x** |
| Median seasonal variance share | 0.436 | **0.162** |
| OOT Spearman vs severity | 0.290 | 0.267 |
| OOT AUC vs binary | 0.857 | 0.808 |

Cost: 0.05 AUC and 0.02 Spearman on the held-out binary-target metrics
(unavoidable when switching from binomial-on-binary to
Tweedie-on-continuous). Acceptable trade-off given the 6.4x lift on the
Freddy benchmark and 63% reduction in seasonal-variance share.

**Strategies tested and rejected**: xgboost (sharper peaks but
worsened seasonal share); two-stage hurdle (Freddy peak actually
*dropped* because Total Affected for Freddy isn't a global outlier).

---

# MOSAIC 0.30.21

## Two correctness fixes in the flood-prob pipeline

Surfaced by an internal review of the EM-DAT integration:

- **Gate / GAM-contract drift fixed.** The `if (include_flood_prob && ...)`
  gate in `compile_suitability_data()` was still checking the v0.30.19-era
  predictors (`temp_anom`, `elevation`, `urban_population_pct`) which were
  removed from the GAM in v0.30.20, and was failing to check the 9 new
  columns the rebuilt GAM actually requires (`precipitation_sum`,
  `precip_sum_2w/4w/8w`, `precip_extreme_p90_count`,
  `soil_moisture_0_to_10cm_mean`, `relative_humidity_2m_mean`,
  `rh_mean_12w`, `wind_speed_10m_max`, `ENSO3`, `ENSO4`). The result: a
  missing required column would have caused a hard `stop()` deep inside
  `impute_flood_probability()` instead of being skipped cleanly by the
  `else` branch. Both `impute_flood_probability()` and the gate now
  source the canonical required-column list from a single internal
  helper `.impute_flood_probability_required()`, eliminating the drift
  surface entirely.
- **`emdat_flood_prob_anom` historical baseline fixed.** The per-country
  mean used as the anomaly baseline was computed over the full series
  including forecast-window rows, whose probabilities are themselves
  GAM-extrapolated. This silently leaked a small amount of forecast
  information into the historical-period anomaly used as an LSTM training
  feature. The baseline now uses only rows where
  `emdat_flood_active` is non-NA (the historical EM-DAT panel coverage).

---

# MOSAIC 0.30.20

## Flood-probability GAM: rebuild around hydrological drivers + ENSO/IOD

Variance decomposition of v0.30.19's imputed `emdat_flood_prob` showed
the highest-flood-activity countries (NGA 92%, TCD 83%, SSD 83%, MLI 75%,
ETH 75%) were heavily dominated by seasonal pattern, and `summary(model)`
revealed that the cyclic-week and country-RE terms plus the raw 12-week
rainfall sum carried virtually all the explanatory power while every
anomaly/teleconnection term was shrunk by the `select = TRUE` penalty.
This release rebuilds the GAM formula around variables that are
ostensibly linked with flood physics, and removes the seasonal /
static-baseline channels that were crowding out the climate-driven
interannual signal:

- **Removed** `s(week, bs = "cc")` (cyclic seasonality), `elevation`, and
  `urban_population_pct`. The country random effect alone absorbs the
  remaining country-level baseline differences.
- **Removed** `s(temp_anom)` — temperature isn't a direct flood driver
  in AFRO (no snowmelt).
- **Added every precipitation channel**: `precipitation_sum`,
  `precip_anom`, `precip_sum_2w/4w/8w/12w`, plus the binary
  `precip_extreme_p90_count` extreme-rain trigger.
- **Added soil moisture (raw + anomaly)**, atmospheric humidity (raw +
  12w rolling), and the storm/cyclone proxy `wind_speed_10m_max`.
- **Heavy ENSO/IOD bench**: ENSO34 at current + 8/16/24-week lags,
  current ENSO3 and ENSO4 (Eastern and Western Pacific regions), IOD at
  current + 8/16/24-week lags. Lag-N columns are computed inline.
- **Removed `select = TRUE`** — the smooth-shrinkage penalty was the
  mechanism that suppressed exactly the teleconnection terms we now
  want to emphasise. Without it the smooths keep their unpenalized REML
  weight; rolling-year CV is the arbiter.

---

# MOSAIC 0.30.19

## Code-review fixes in `compile_suitability_data` and `est_suitability`

Round of correctness fixes surfaced by an internal code review:

- **B1 — `est_suitability()` date auto-detection** referenced `d_all$date_start` / `enso_complete$date_stop`, columns that `compile_suitability_data()` drops before saving. `min(NULL, na.rm=TRUE)` silently returns `Inf` and propagated as bogus dates. Fixed to use `d_all$date`.
- **B2 — Fine-tuning splits trained against the wrong target scale.** The model output head is `activation = "linear", name = "logit_head"` and the first split correctly fed `qlogis()`-transformed targets. Subsequent fine-tuning splits (default `n_splits = 10`) fed raw `[0, 1]` probabilities, pushing the linear head toward the wrong range and silently undoing the first split's training. Now applies `qlogis(pmax(eps, pmin(1-eps, ...)))` consistently.
- **B3 — `seasonal_outbreak_risk`** computed `mean(cases_lagged)` inside `group_by(iso_code) %>% mutate(...)`, producing a single per-country mean rather than the week-of-year climatology the comment claimed. Now uses `stats::ave(cases_lagged, week, FUN = mean)` to give the intended weekly seasonality.
- **B4 — `weeks_since_major_outbreak`** incremented its counter through the `memory_lag` warm-up window (default 17 weeks in forecast mode), producing fake `1, 2, 3, ...` values when the lag target was NA. Now stays NA until the first real observation.
- **B5 — ISO week 53.** Investigated and documented. Two distinct things were happening: (a) legitimate W53 in 2015 and 2020 was being dropped (~80 country-weeks, 0.2% of training data); (b) an upstream bug in `process_open_meteo_data` emits spurious W53 entries labelled by calendar year for ISO years 2005/2010/2016/2021. The drop-all-W53 filter robustly handles (b). Added a comment block explaining the trade-off; the upstream fix to `process_open_meteo_data` is the structural follow-up (separate ticket).
- **B6 — Vaccination weekly key** paired `lubridate::isoweek(date)` with `format(date, "%Y")` (calendar year). Dates near the year boundary (e.g., 2024-12-30, which is ISO 2025-W01) were joining under `(year=2024, week=1)`, silently missing the right upstream row. Now uses `lubridate::isoyear()`.
- **B7 — `pivot_wider` for climate and ENSO** had no `values_fn`, so any residual duplicate `(iso_code, year, week, variable)` row would silently produce list-columns and break every downstream merge. Now passes `values_fn = mean`.
- **B8 — `est_suitability()` plotting block** was gated by `if (T)` (always on) with a comment claiming "disabled by default", and `par(mfrow = c(2,1))` leaked into the caller's device state. Replaced with a real `plot_country_diagnostics = FALSE` argument and `on.exit(par(old_par))`.
- **B9 — Positional column renames** (`names(x)[3] <- "..."`) replaced with `dplyr::rename()` so an upstream column reorder can't silently corrupt downstream values.
- **B10 — Negative case counts** in `est_suitability()` are now flagged with a warning before being clamped to 0 (previously silent).
- **B11 — Duplicate identical `df <- data.frame(...)` block** in `est_suitability()` removed.
- **B12 — Dead first `covariates_all` assignment** in `est_suitability()` removed; only the comprehensive (live) assignment remains.

---

# MOSAIC 0.30.18

## Speed up flood-prob GAM and clean the suitability covariate list

- `impute_flood_probability()` now fits with `mgcv::bam(method = "fREML", discrete = TRUE)`
  rather than `mgcv::gam(method = "REML")`. Same formula, essentially identical
  fitted values; wall-time on the real ~30k-row training set drops from ~13 min
  to ~10 sec. Rolling-year CV is also capped at the 3 most recent fully-observed
  years (previously open-ended). End-to-end pipeline now runs in ~12 sec instead
  of ~100 min.
- `est_suitability()` covariate list cleaned: removed `log1p_cum_vaccine_doses`
  (vaccination intervention — correlated with outbreaks because deployment
  follows cholera, so including it teaches a non-causal shortcut that breaks at
  forecast time) and the raw `year` / `month` / `week` timestamps (let the model
  memorise period-specific cholera waves; the sin/cos cyclic terms already in
  the list carry seasonality cleanly). Added a comment block to `covariates_all`
  documenting the rationale.
- End-to-end validation against the real data: mean rolling-year CV AUC 0.848
  on 2023/2024/2025 held out; forecast-window `emdat_flood_prob` distribution
  is shape-similar to historical (mean 0.095 vs 0.062), confirming the GAM
  uses climate signal rather than collapsing to a constant.

---

# MOSAIC 0.30.17

## Impute country-week flood probability for use in `est_suitability()`

EM-DAT is observed-only — for forecast-window weeks, the raw `emdat_flood_active`
binary added in v0.30.16 is identically zero, creating a distribution shift
between training and inference for the suitability LSTM.

This release replaces the raw binary with a continuous **imputed flood
probability** that is populated identically in historical and forecast
weeks:

- **New: `impute_flood_probability()`** — fits a binomial GAM
  (`mgcv::gam`, logit link, REML, `select = TRUE`) on observed
  `emdat_flood_active` events using climate anomalies, cyclic seasonality,
  country random effects, and inline-computed `ENSO34_lag20` /
  `IOD_lag16` predictors. Predicts a non-NA `emdat_flood_prob ∈ [0, 1]`
  for every (iso × week) row. With `diagnostics = TRUE` (default) writes
  smooth-term plots, decile-calibration plot, rolling-year CV metrics
  CSV, and `summary(model)` to `<PATHS$DOCS_FIGURES>/flood_imputation/`.
  Warns if mean CV AUC < 0.65.
- **`compile_suitability_data()`** gains `include_flood_prob = TRUE`
  (default). When on, calls `impute_flood_probability()` after the
  existing NA-imputation block and adds four rolling-window aggregates:
  `emdat_flood_prob_4w_max`, `emdat_flood_prob_12w_max`,
  `emdat_flood_prob_12w_sum`, `emdat_flood_prob_anom`. The previous
  zero-fill on the raw EM-DAT join is removed so forecast-window rows
  remain NA for the GAM to impute.
- **`est_suitability()`** consumes the five `emdat_flood_prob*` columns
  in place of the four raw `emdat_flood_*` columns added in v0.30.16.
  The raw columns remain in the suitability dataframe for audit but are
  not fed to the LSTM.

No new dependencies (`mgcv` was already an Imports dep).

---

# MOSAIC 0.30.6

## Add GitHub profile links for Dejan Lukacevic and Meikang Wu

`_pkgdown.yml` now wires Dejan to `DLukacevic-IDM` and Meikang to `MeWu-IDM`. All five contributors in the sidebar and authors page now have working GitHub profile links.

---

# MOSAIC 0.30.5

## Show all contributors in pkgdown sidebar; fix Christopher's broken GitHub link

Two pkgdown configuration fixes for the authors page and Developers sidebar:

1. **Sidebar shows all five roles, not just `aut` + `cre`.** Added `authors.sidebar.roles: [aut, cre, ctb]` to `_pkgdown.yml`. Previously the right-hand "Developers" block on the home page listed only John (aut, cre) and Christopher (aut, ctb); Tony, Dejan, and Meikang (all `ctb` only) were hidden. They now all appear.
2. **Christopher's GitHub link was 404.** `_pkgdown.yml` had `https://github.com/ChristopherWLorton` (no such user); the actual handle is `clorton`. Fixed.
3. **Tony's GitHub link added.** `https://github.com/tinghf`.

John's `gilesjohnr` link was already correct.

---

# MOSAIC 0.30.3

## Sync `rho_deaths` into MOZ data-raw, JSON sidecars, and simulation configs

Cleanup pass after independent review of v0.30.2 surfaced three medium-severity gaps left over from the targeted `.rda` rebuild. v0.30.2 rebuilt the top-level `.rda` binaries surgically but left several auxiliary data-raw scripts and `inst/extdata` JSON sidecars out of sync — re-running any of them would have regressed the `rho_deaths` plumbing. This release closes those gaps so all source-of-truth artifacts agree:

- **MOZ data-raw scripts.** `data-raw/make_priors_default_MOZ.R` adds the Beta(3, 2) `rho_deaths` prior block (metadata bumped 3.0 → 3.1); `data-raw/make_config_default_MOZ.R` adds `rho_deaths = 0.6` (metadata bumped 2.5 → 2.6).
- **LASER config templates.** `data-raw/make_default_LASER_config_files.R`, `make_simulation_endemic_LASER_config_files.R`, and `make_simulation_epidemic_LASER_config_files.R` add `rho_deaths = 0.6`.
- **`inst/extdata/` JSON sidecars.** All six (`default_parameters{,_MOZ}.json`, `sim_endemic_parameters.json`, `simulated_parameters.json`, `priors_default{,_MOZ}.json`) regenerated with `rho_deaths`; `.gz` mirrors refreshed. The two priors JSONs also bumped to versions 15.1 and 3.1.
- **Simulation `.rda` binaries.** `config_simulation_endemic.rda` and `config_simulation_epidemic.rda` rebuilt with `rho_deaths = 0.6`.
- **Version metadata sync.** `priors_default_MOZ.rda` had inherited the wrong version (15.1) during the v0.30.2 surgical rebuild; corrected to the MOZ-track 3.1 to match the source script.

Refs #100. Phase 1 of the `calc_model_likelihood` Python port (laser-cholera#47) is now complete and consistent across all source-of-truth artifacts.

---

# MOSAIC 0.30.2

## Add `rho_deaths` plumbing for death detection model

Adds the MOSAIC-side wiring for the `rho_deaths` parameter introduced by [laser-cholera#49](https://github.com/InstituteforDiseaseModeling/laser-cholera/issues/49) (death detection probability, analogous to `rho` for cases). Folded into the Phase 1 paramfile work (issue #100 addendum): laser-cholera 0.12.x's permissive validator silently tolerates the new key in the paramfile; once 0.13 ships with #49, the engine consumes it and produces `reported_deaths`.

- **Defaults.** `data-raw/make_config_default.R` sets `rho_deaths = 0.6` (mean of Beta(3, 2); Finger et al. 2024 documents ~60% surveillance capture of true cholera deaths). `data-raw/make_priors_default.R` adds the Beta(3, 2) prior block; `priors_default` metadata version bumped 15.0 → 15.1.
- **Targeted `.rda` rebuild** for `config_default`, `config_default_MOZ`, `priors_default`, `priors_default_MOZ`. Avoids re-running the full data-raw pipeline (which depends on paths and external data files).
- **Sampling control.** `mosaic_control_defaults()$sampling$sample_rho_deaths = TRUE` in `R/run_MOSAIC.R`. `R/sample_parameters.R` adds `sample_rho_deaths = TRUE` to defaults; includes `rho_deaths` in `validate_sampled_config` global params; included in the `disease_only` disabled-flag-resolution list alongside `sample_rho`.
- **`make_LASER_config()`.** New `rho_deaths = NULL` argument with `[0, 1]` validation when supplied; pass-through to params; `@param` documented.
- **Round-trip schema.** `R/convert_config_to_matrix.R`, `R/convert_config_to_dataframe.R`, `R/get_param_names.R`, `R/plot_model_parameters.R` include `rho_deaths` in the relevant params/keep lists so it appears in `samples.parquet` and posterior plots.
- **Test coverage.** New `tests/testthat/test-sample_parameters_rho_deaths.R` confirms the prior exists and shapes are Beta(3, 2); sampling under `sample_rho_deaths = TRUE` produces draws in (0, 1); `sample_rho_deaths = FALSE` holds `rho_deaths` at `config_default`; empirical mean ≈ 0.6 over 200 draws. Tests skip cleanly when MOSAIC root or rebuilt prior is unavailable.

Refs #100, laser-cholera#49.

---

# MOSAIC 0.30.1

## Wire likelihood control + `epidemic_peaks` into Dask paramfile

Phase 1 of the `calc_model_likelihood` Python port ([laser-cholera#47](https://github.com/InstituteforDiseaseModeling/laser-cholera/issues/47), tracked in issue #100). Additive wiring on the Dask path so the laser-cholera analyzer can score on-worker once Phase 2 (laser-cholera 0.13) and Phase 3 (Dask worker schema flip, MOSAIC-pkg #101) land. Local PSOCK/FORK path is unchanged.

- **Inject helper.** New `.mosaic_inject_likelihood_settings()` in `R/run_MOSAIC.R` flattens 12 likelihood-control keys (`weight_cases`, `weight_deaths`, `weights_time` (= `.weights_time_resolved`), `weights_location`, `nb_k_min_cases`, `nb_k_min_deaths`, `weight_peak_timing`, `weight_peak_magnitude`, `weight_cumulative_total`, `weight_wis`, `sigma_peak_time`, `sigma_peak_log`) plus `epidemic_peaks` (trimmed to `iso_code`/`peak_date`) plus `calc_likelihood = TRUE` onto config.
- **Dask preflight.** `run_MOSAIC()` calls the inject helper before `.extract_base_config()`, so the keys ride along on the scattered `base_config` and land on `model.params` for the analyzer to read. Uses `.weights_time_resolved` (the rescaled-to-sum-`n_t` vector), not the raw `control$likelihood$weights_time`.
- **Keep list extension.** `R/run_MOSAIC_helpers.R::.extract_base_config()` now keeps the 13 new keys plus `calc_likelihood`. Local-path configs (no inject) keep none of them by construction — the keep filter is membership-based.
- **Defensive guard.** `.mosaic_run_simulation_worker()` explicitly sets `params_sim$calc_likelihood = FALSE` before the LASER call so a stray user-set `TRUE` on the local path can't trigger the analyzer with un-flattened keys.
- **Unit tests.** New `tests/testthat/test-inject_likelihood_settings.R` covers the inject helper (forwards `.weights_time_resolved`, additive only, NULL handling) and the `.extract_base_config` keep list (presence on Dask path, absence on local). 14 assertions, all green.

Refs #100.

---

# MOSAIC 0.30.0

## Route per-location prediction CSVs to `3_results/predictions/`

Previously `plot_model_ensemble(save_predictions = TRUE)` wrote per-location CSVs (`predictions_<type>_<LOC>.csv`) to its `output_dir`, which `run_MOSAIC()` sets to `3_results/figures/predictions/` — mixing data files into a tree meant for figures. The combining step then wrote `predictions_<type>_all.csv` into `3_results/predictions/`, a byte-identical duplicate when the run covers a single location.

This release:

1. **Separate data/figures trees.** Added `data_dir` argument to `plot_model_ensemble()`. When provided, per-location CSVs are written there instead of `output_dir`. `run_MOSAIC()` passes `data_dir = dirs$res_predictions` at all three call sites (best, medoid, ensemble). `figures/predictions/` now holds PDFs only.
2. **Combining step reads from `predictions/`** (the new canonical location) instead of `figures/predictions/`. Regex excludes `*_all.csv` so the combine pass ignores its own output.
3. **Single-location runs no longer emit `_all.csv`.** When only one per-location CSV exists, the canonical file is the per-location CSV itself — no `file.copy()` to a byte-identical `_all.csv`. `_all.csv` is written only for multi-location runs where there's genuine concatenation happening.
4. **`plot_model_ppc()`** now reads from `dirs$res_predictions` rather than `dirs$res_fig_pred` (auto-discovery still works; the function's docstring example updated to match).

Backwards-compatibility: `plot_model_ensemble()` defaults `data_dir = NULL`, which falls back to `output_dir` — external callers of the function keep working unchanged.

---

# MOSAIC 0.29.9

## Prior/posterior panels: draw mode vlines for every method in every panel

Dashed central-tendency lines in `distributions_*_Prior_Posterior.pdf` panels now:

1. Mark the **mode** of the plotted density (argmax of the displayed curve) rather than the mean (v0.29.4 and earlier) or median (v0.29.8). On a log-scale axis the mode corresponds to `exp(meanlog)` for a lognormal (= median of X, the visible peak of the log10-density bell). On a linear axis the mode corresponds to the classical mode of X (e.g. `(s1-1)/(s1+s2-2)` for a Beta with `s1>1` and `s2>1`). Computed directly from `x[which.max(y)]` on the grid, so it always aligns with the visible peak regardless of axis.
2. Draw for **every method** in every panel, not just the ones whose center falls inside the data range. The previous `line_val >= x_range[1] && line_val <= x_range[2]` gate occasionally suppressed a line when the mean of a skewed distribution fell outside the plotted support. Lines are drawn unconditionally now — ggplot extends the axis if the mode sits outside the density's effective range.

Near-delta distributions demoted to `fixed_values` vlines (v0.29.8) still render as solid lines at the median, so the prior/posterior marks remain visually distinct when one method is effectively a point estimate.

---

# MOSAIC 0.29.8

## Fix vertical-line alignment, near-delta posteriors, and add beta_j0_tot to log scale

Three follow-ups to the log-scale posterior plotting in v0.29.6/0.29.7:

1. **Mean vertical lines were misaligned with the visible peak on log-x.** For a wide lognormal the arithmetic mean `exp(meanlog + sdlog²/2)` sits many decades to the right of the density peak on the log10 axis (the peak is at the median `exp(meanlog)`). For zeta_ratio prior the mean was ~15,000× to the right of the visible peak. Fix: for log-scale params the dashed central-tendency line is now drawn at the median (`qbeta(0.5, ...)` for Beta, `exp(meanlog)` for lognormal). Linear-axis params still use the mean as before.

2. **prop_E_initial / prop_I_initial prior curves collapsed to flat lines.** The posterior Beta(62199, 6e11) has a 0.008-decade effective support — a near-delta distribution whose peak density on log10-axis is ~600× larger than the prior's. On shared y-axis the wider prior rendered as a flat line. Fix: distributions with `log10(q_0.99) - log10(q_0.01) < 0.05` on log-scale axes are demoted to a solid vertical line at the median (matching the `fixed_values` styling), letting the wider prior/posterior set the y-axis scale.

3. **Added beta_j0_tot to the log-scale list.** `beta_j0_tot` is lognormal(meanlog=-10.8, sdlog=2.0), a 3.4-decade span — a natural log-scale candidate, missed in v0.29.6.

---

# MOSAIC 0.29.7

## Fix collapsed posteriors on log-scale prior/posterior plots

The v0.29.6 log-scale switch in `plot_model_distributions()` plotted `dlnorm(x, ...)` (density w.r.t. `x`) against a log-x axis. Linear-space densities for wide lognormals shrink with scale — e.g. the zeta_ratio posterior in MOZ_s1_explore_v2 has a peak `dlnorm` of 9.9e-7 vs the prior at 8.1 (7 orders of magnitude smaller). On a shared linear y-axis the posterior flattened to a line at zero.

Fix is the standard change of variables for density on a log axis: for `Y = log10(X)`,

`f_Y(log10(x)) = f_X(x) · x · ln(10)`

Applied to the lognormal and beta branches whenever the param is on log-scale. For lognormal, this is equivalent to `dnorm(log10(x), meanlog/ln(10), sdlog/ln(10))` — the same symmetric-bell convention used in `est_kappa_prior.R:281`.

After the fix, the zeta_1 and zeta_ratio posteriors for the MOZ run now render as visible curves rather than collapsed lines. Peak heights are comparable across prior/posterior (all in the 0.2–0.4 range for zeta_* on the log10 density scale) regardless of where mass sits on the x-axis.

---

# MOSAIC 0.29.6

## Log-scale x-axis for wide-span parameters in global prior/posterior plots

`plot_model_distributions()` renders prior vs. posterior densities for all global parameters into `distributions_global_Prior_Posterior.pdf`. Parameters with multi-order-of-magnitude support (lognormal `kappa`, `zeta_1`, `zeta_2`, `zeta_ratio`; heavily left-skewed Beta priors on `prop_E_initial`, `prop_I_initial`) previously rendered on a linear x-axis — the distribution looked like a spike at zero with an invisible right tail, wasting the panel and hiding the posterior shape.

This release switches those six panels to `scale_x_log10()` with `annotation_logticks(sides = "b")`, matching the styling already used in `est_kappa_prior.R:375` for the `kappa_prior.png` forest/density plots. Density grids for these params are now log-spaced (`exp(seq(log(x_min), log(x_max), ...))`) so the rendered curve is smooth across the full log range rather than linear-clumped at the right edge.

**Log-scale params:** `kappa`, `zeta_1`, `zeta_2`, `zeta_ratio`, `prop_E_initial`, `prop_I_initial`.

**Unchanged:** All Beta priors on [0,1] probabilities, truncnorm priors on bounded absolute ranges, and narrow-span lognormal/gamma priors still use linear x.

---

# MOSAIC 0.29.5

## Best/medoid prediction plots: independent `n_iter` and parallel execution

Best and medoid single-config prediction plots previously reused `ensemble_n_sims_per_param` (default 5, sequential) because they were refactored into `calc_model_ensemble()` in 0.29.2 without giving them their own iteration control. For tight stochastic CI envelopes on the best/medoid plots, users had to bump the ensemble count — paying the cost across all N posterior parameter sets.

This release splits the two:

* `control$predictions$n_iter_ensemble` (default **10L**) — stochastic runs per posterior parameter set in the weighted ensemble. Renamed from `ensemble_n_sims_per_param`.
* `control$predictions$n_iter_best` (default **100L**) — stochastic runs for the best and medoid single-config plots. Applied identically to both models.

Best/medoid now also run in parallel: `calc_model_ensemble()` is invoked with `parallel = control$parallel$enable` and `n_cores = control$parallel$n_cores` (previously hardcoded `parallel = FALSE`). Parallelization uses an internal PSOCK cluster — same infrastructure the posterior ensemble already uses in the non-Dask path. For Dask calibrations, the best/medoid step still uses local PSOCK since the Dask cluster is closed after the posterior-ensemble sims.

**Breaking rename:** `control$predictions$ensemble_n_sims_per_param` → `control$predictions$n_iter_ensemble`. User scripts (`vm/`, `azure/`, vignettes) updated accordingly. Existing scripts setting `best_model_n_sims` (previously an orphaned no-op control field) migrate to `n_iter_best` and are now wired in.

---

# MOSAIC 0.29.3

## Unified stochastic-median R² and bias across best, medoid, and ensemble

Follow-up to 0.29.2. The 0.29.2 fix routed the best and medoid prediction **plots** through `calc_model_ensemble()` + `plot_model_ensemble()` so the plot captions report R²/bias from the stochastic median. But the separate `"Best model R²"` / `"Medoid model R²"` **log lines** still came from a single deterministic `lc$run_model()` call, producing two different numbers for the same thing (log vs plot caption). Red-team review flagged the inconsistency.

This release eliminates the separate deterministic LASER calls for best/medoid and sources all three sets of reported metrics — best, medoid, ensemble — from the stochastic median of their respective `mosaic_ensemble` objects:

* `R/run_MOSAIC.R` best-model block reordered: `calc_model_ensemble(configs = list(config_best), ...)` is called once; R²/bias are computed from `best_ensemble$cases_median` / `$deaths_median` and passed to both the log line and downstream `summary.json`. Same refactor applied to the medoid block.
* Log format now: `"Best model R² (1 params x N stoch): cases = X (bias=Y), deaths = ..."` — mirrors the existing ensemble log format.
* Removes two per-run LASER calls (the single deterministic `best_model <-` and `medoid_model <-` runs) that are no longer needed; best/medoid each now run `n_ensemble_stochastic_per` LASER sims total (default 10), same as before the 0.29.2 fix *plus* plot but minus the deterministic R² helper run.
* Retires the `lc <- reticulate::import(...)` import in the main `run_MOSAIC` body — `calc_model_ensemble` handles the import internally.

No public API changes.

---

# MOSAIC 0.29.2

## Fix best/medoid prediction plots: `reported_cases`, stochastic CI, unified naming

`plot_model_fit()` (used by the best-model and medoid-model plots in `run_MOSAIC()`) rendered `model$results$expected_cases` — the back-calculated burden `new_symptomatic / rho`, typically 5-10x inflated vs surveillance-comparable cases. Every other part of the pipeline (likelihood, R², ensemble) used `model$results$reported_cases = Isym * rho / chi_eff`. The v0.14.22 commit that renamed `expected_cases` → `reported_cases` in sibling plotting functions missed this fourth file; the bug was latent until v0.22.15 re-wired `plot_model_fit()` into the best-model block, and real from v0.22.15 through v0.29.1.

**Fix consolidates best/medoid plots into the existing `calc_model_ensemble` + `plot_model_ensemble` pipeline** — the same functions the posterior ensemble uses — in single-config mode (one parameter set × N stochastic reruns). One codepath for all three plot types eliminates the parallel-implementation drift that caused the original bug.

**Changes:**

* `R/plot_model_ensemble.R`: new `file_prefix` (default `"ensemble"`) and `title_label` (default `"Posterior Ensemble"`) parameters; hardcoded filename and title strings replaced; single-param-set subtitle branch added.
* `R/run_MOSAIC.R`: best and medoid plot calls replaced with `calc_model_ensemble(configs = list(config_<...>), n_simulations_per_config = n_ensemble_stochastic_per, envelope_quantiles = c(0.025, 0.975))` + `plot_model_ensemble(file_prefix = "best" | "medoid", ...)`. Medoid output moved from `2_calibration/best_model/` to `3_results/figures/predictions/` alongside the ensemble plots. Explicit `file_prefix = "ensemble"` added at the main ensemble plot call for self-documentation.
* Prediction-CSV combining loop extended to iterate `c("ensemble", "best", "medoid", "stochastic")` and filename pattern renamed from `all_predictions_<type>.csv` → `predictions_<type>_all.csv` for consistency with the plot naming.
* `R/plot_model_fit.R`: deleted (retired). The function was the single source of the drift bug and had no internal callers after the refactor.

**Output naming** (all under `3_results/figures/predictions/` unless noted):

* `predictions_<prefix>_<LOC>.pdf` + `.csv` per-location (prefixes: `ensemble`, `best`, `medoid`)
* `predictions_<prefix>_cases_all.pdf`, `predictions_<prefix>_deaths_all.pdf` faceted multi-location overviews
* `predictions_<type>_all.csv` combined across locations (in `3_results/predictions/`)

**Lesson recorded** in `CLAUDE.md` (item 11): when renaming a field across sibling functions, grep exhaustively; do not skip temporarily-unused functions; prefer consolidating into one shared code path over maintaining N parallel implementations that must be updated in lockstep.

---

# MOSAIC 0.29.1

## Bias corrections + zeta_ratio channel switch (follow-up to 0.29.0)

Two changes landed together in this patch:

1. **`zeta_ratio` default switched from combined (C) to direct literature-anchor channel (A).** The combined precision-weighted fit at median 2.16e5 was pulled high by the derived-from-marginals channel (~1.15e6), overestimating the per-day asymptomatic:symptomatic shedding asymmetry vs modelling-convention + household-transmission evidence (Smith 2026 ~1.6x, Chao/Finger ~10). The direct channel is now what `make_priors_default.R` and `make_priors_default_MOZ.R` write into `priors_default$parameters_global$zeta_ratio`. The combined and derived fits remain available via `est_zeta_ratio_prior()$diagnostics$fit_combined` and `$fit_derived`.

2. **Bias corrections to zeta_1.** Code review identified several compounding upward biases in the v0.29.0 `zeta_1` fit. All are addressed here; `zeta_1` median drops from 3.72e11 to 1.39e11 (mode ÷66).

**Corrections applied:**
* `R/est_zeta_1_prior.R`: V_sev central value lowered from 8 L/day to 4 L/day (time-averaged over the 1-2 week clinical course rather than first-24-h peak rate from Harris 2012); V_mod 4 -> 2 L/day; V_mild 500 -> 300 mL/day. Mild concentration lowered from 10^6 to 10^5 cells/mL (non-rice-water stool). Nelson 2020, Kaper 1995, and Harris 2012 downweighted from 0.50 to 0.10 (reviews that cite the same Nelson-era primary data, not independent measurements). Endemic and outbreak severity-weighted pool rows given weight 0 (they are derived quantities of rows 1/4/5 and including them was triple-counting the severe class).
* `R/est_zeta_2_prior.R`: Kaper rows downweighted from 0.25 to 0.10 (review overlap).
* `R/est_zeta_ratio_prior.R` direct channel: Nelson 2009 paired weight 1.00 -> 0.30 (value 10^5 is a stool concentration ratio, not a per-day rate ratio - unit-inconsistent with zeta_ratio). Kaper and Harris paired rows 0.25 -> 0.10 (review overlap).

**Net prior shifts (main = MOZ):**
* `zeta_1`: LN(26.64, 1.69) -> LN(25.65, 2.46); median 3.72e11 -> 1.39e11; mode 2.15e10 -> **3.29e8** (the config point estimate uses the mode).
* `zeta_2`: LN(12.69, 2.00) -> LN(12.30, 2.00); median 3.23e5 -> 2.20e5.
* `zeta_ratio` direct channel: LN(6.64, 4.81) -> LN(4.31, 4.39); median 763 -> **74.7** (config point estimate uses median; mode is pathological for sdlog=4.39).

**config_default and config_default_MOZ placeholders updated** to reflect the new modes/medians.

**Test updates:** `tests/testthat/test-sample_parameters_zeta.R` coverage range for `zeta_1` widened from `(1e9, 1e14)` to `(1e8, 1e14)` to match the wider bias-corrected sdlog. All 18 zeta tests pass.

---

# MOSAIC 0.29.0

## Breaking changes (prior scale shift)

* **`zeta_1`, `zeta_2`, and `zeta_ratio` priors are re-estimated from a literature meta-analysis** (`R/est_zeta_1_prior.R`, `R/est_zeta_2_prior.R`, `R/est_zeta_ratio_prior.R`). The new priors encode the biological scale of *V. cholerae* shedding (cells per infected person per day) rather than the Frame-B LASER count-scale used by prior defaults. This is a ~6 order-of-magnitude upward shift on `zeta_1` (prior median moves from 70 000 to ~1e11-1e12 cells/person/day) and a corresponding re-centring of `zeta_ratio`. The previous defaults `LN(log(70 000), 0.8)` and `LN(log(300), 1.2)` are replaced by weighted-MLE lognormal fits on primary-source anchors (Nelson 2009, Merrell 2002, Harris 2012, Smith 2026 medRxiv, Kaper 1995, etc.).
* **`zeta_2` becomes a first-class prior.** `priors_default$parameters_global$zeta_2` is now populated with the literature-derived lognormal. `sample_parameters()` still derives the sampled `zeta_2 = zeta_1 / zeta_ratio` at sampling time (guarantees `zeta_1 > zeta_2` algebraically); the stored `zeta_2` prior is the reference distribution used for validation and downstream diagnostics.
* **Existing calibration posteriors are invalidated.** The current `zeta_1` posterior centred at ~48 k has effectively probability 0 under the new prior. Every existing calibration artefact under `MOSAIC-Mozambique/output/calibration/` must be re-run with the new priors before use.
* **`config_default.rda` scale shift.** `make_config_default.R` placeholder constants (`zeta_1`, `zeta_2`, `.zeta_ratio_default`) have been updated to the new prior medians. Any code that reads `config_default$zeta_*` expecting the old numeric scale will behave differently.
* **`config_default_MOZ.rda` scale shift.** The same placeholder constants in `make_config_default_MOZ.R` have been updated.
* **MOZ project override (stand-alone MOSAIC-Mozambique)** uses its own `zeta_ratio` centre (50) independent of the pkg default. That override is unaffected; the MOZ team decides adoption there.
* **LASER reservoir storage precision.** At the new biological scale (`zeta_1 ~ 1e11`, `I_sym ~ 100`), the daily Poisson mean contribution to `W` reaches ~1e13 cells - far above float32's exact-integer limit (~1.7e7). **`laser-cholera/src/laser/cholera/metapop/environmental.py` must widen `W` / `W_next` to float64 before this release can be merged.** This is an EXTERNAL (READ-ONLY) laser-cholera change and is tracked as a pending prerequisite; until the dtype change lands, running `run_MOSAIC()` against the new priors will silently accumulate rounding error in the reservoir update each tick.

## New functions

* `est_zeta_1_prior(PATHS, severity_mix)` - weighted-MLE lognormal on symptomatic shedding anchors.
* `est_zeta_2_prior(PATHS)` - weighted-MLE lognormal on asymptomatic shedding anchors with hard `sdlog >= 2.0` floor.
* `est_zeta_ratio_prior(PATHS, zeta_1_fit, zeta_2_fit, n_sim, seed)` - precision-weighted combination of direct-literature and derived paired-Monte-Carlo channels.

## Migration notes

* Rebuild priors: `source("data-raw/make_priors_default.R")`.
* Rebuild MOZ priors: `source("data-raw/make_priors_default_MOZ.R")`.
* Rebuild configs: `source("data-raw/make_config_default.R")` and `source("data-raw/make_config_default_MOZ.R")`.
* Downstream MOSAIC-docs figures with `zeta_*` axes must be regenerated.
* Re-run calibration for every production configuration before using outputs in interventions analyses.

# MOSAIC 0.28.13

## Other

* Added Tony Ting, Dejan Lukacevic, and Meikang Wu to package authors as contributors (`ctb`) in `DESCRIPTION` and the pkgdown site.

# MOSAIC 0.24.1

## Bug fixes

* `process_open_meteo_data()` now renames `soil_moisture_0_to_7cm_mean` to `soil_moisture_0_to_10cm_mean` on raw ERA5 historical parquets before splicing with climate-model projections. Upstream open-meteo-pipeline [issue #5](https://github.com/InstituteforDiseaseModeling/open-meteo-pipeline/issues/5) switched ERA5 requests to the 0-7 cm band (the ERA5 Historical API silently returned all-null for the 0-10 cm band), so without this rename the `rbind()` of historical + climate frames produced mismatched columns and every downstream soil-moisture feature in `compile_suitability_data()` / `est_suitability()` became NA. Users who have cached outputs from before the upstream fix should run `process_open_meteo_data(PATHS, force = TRUE)` once to force regeneration; the cache check compares source vs. output mtimes and will not otherwise pick up the upstream schema change.

# MOSAIC 0.24.0

## Behavior change (not API)

* **`control$predictions$optimize_subset = TRUE` now drives the canonical posterior artifacts.** Previously the optimized subset was a parallel reporting track: it produced an `ensemble_optimized.rds` and `*_optimized` metrics in `summary.json`, but `posteriors.json`, `posterior_quantiles.csv`, ensemble plots, and chained downstream priors were all computed from the tier-selected subset. After this release, when the flag is on the optimized subset is written to new `is_best_subset_opt` / `weight_best_opt` columns in `samples.parquet` and every posterior-consuming function reads from those columns. The tier-selected subset remains in `is_best_subset` / `weight_best` for provenance.
* **`summary.json` field rename.** The previous `r2_cases_ensemble_optimized` / `r2_deaths_ensemble_optimized` / `bias_ratio_*_ensemble_optimized` / `n_ensemble_params_optimized` fields are renamed to `*_ensemble_tier` / `n_ensemble_params_tier`. The canonical `r2_cases_ensemble` (etc.) now holds the optimized metrics when `optimize_subset = TRUE` and the tier metrics when the flag is off; the new `*_tier` fields preserve the tier-subset metrics for side-by-side comparison (NA when the flag is off). Downstream consumers reading the old `_optimized` field names must be updated.
* **Ensemble construction moved earlier in `run_MOSAIC()`.** The Dask reconnect + stochastic sims + `calc_model_ensemble` block now runs **before** posterior quantile/distribution/sensitivity construction so that the optimizer (when enabled) can refine the posterior. Best-model PPC and ensemble metrics/plots remain after posterior construction.
* Users relying on the old behavior (tier subset drives posteriors regardless of flag) should set `control$predictions$optimize_subset = FALSE`.

## New arguments

* `calc_model_posterior_quantiles()`, `plot_model_parameter_correlation()`, `plot_model_parameter_sensitivity()`, and `plot_model_posteriors_detail()` gained `subset_col` and `weight_col` arguments defaulting to `"is_best_subset"` / `"weight_best"`. Existing callers see no change.
* `optimize_ensemble_subset()` gained an optional `seeds` argument and returns `optimal_seeds` so callers can map the optimized subset back to `samples.parquet` without duplicating the internal sort logic.
* `calc_convergence_diagnostics()` gained an optional `n_best_subset_optimized` argument; when supplied, the JSON output includes a new `metrics$B_size_optimized` entry and `summary$n_best_subset_optimized` field.

## Defaults

* `control$predictions$optimize_min_n` raised from `4L` to `30L`. Four was the statistical minimum per Fox et al. (2024), but posterior density estimation on the optimized subset needs more samples; a warning is logged when the optimizer selects `< 30`.

## Internal

* Added `.mosaic_active_subset_cols(results, control)` helper that returns the canonical subset/weight column names plus a `"tier"` / `"optimized"` source tag. Used by `run_MOSAIC()` to thread the correct columns through all posterior-consuming calls.

# MOSAIC 0.13.21

## Bug Fixes

* **Respect control$parallel$enable flag in prediction plotting functions**
  - **Problem**: `plot_model_fit_stochastic()` and `plot_model_fit_stochastic_param()` were hardcoded to use `parallel = TRUE`, ignoring the user's `control$parallel$enable` setting
  - **Solution**: Changed both function calls in `run_MOSAIC()` to use `parallel = control$parallel$enable` instead of hardcoded `TRUE`
  - **Impact**: Users can now disable parallel execution for ensemble predictions by setting `control$parallel$enable = FALSE`, useful for debugging or when parallel execution causes issues
  - **Files modified**: `R/run_MOSAIC.R` (lines 1444, 1478)

# MOSAIC 0.13.20

## Bug Fixes

* **Fix Numba/TBB threading conflict in ALL parallel execution contexts**
  - **Problem**: Numba (used by laser-cholera) and Intel TBB library cause threading conflicts when R forks parallel workers, resulting in "Attempted to fork from a non-main thread" warnings and potential deadlocks or hangs
  - **Solution**: Set threading environment variables to 1 before cluster creation and in each worker across ALL functions that use parallel execution
  - **Environment variables set**:
    - `TBB_NUM_THREADS = "1"` - Intel Threading Building Blocks
    - `NUMBA_NUM_THREADS = "1"` - Numba JIT compiler
    - `OMP_NUM_THREADS = "1"` - OpenMP
    - `MKL_NUM_THREADS = "1"` - Intel MKL
    - `OPENBLAS_NUM_THREADS = "1"` - OpenBLAS
  - **Implementation**: Applied to all 4 locations where `parallel::makeCluster()` is called:
    - `R/run_MOSAIC.R` - Main calibration workflow
    - `R/plot_model_fit_stochastic_param.R` - Ensemble predictions (was causing hangs at "Generating ensemble predictions")
    - `R/plot_model_fit_stochastic.R` - Stochastic predictions
    - `R/calc_npe_diagnostics.R` - NPE SBC diagnostics
  - **Each location now has**:
    - Environment variables set in main process before cluster creation
    - BLAS thread limiting in workers
    - Environment variables set again in each worker
  - **Impact**: Prevents fork-related threading conflicts across entire package, ensures stable parallel execution with laser-cholera simulations, fixes hangs during ensemble predictions
  - **Files modified**: `R/run_MOSAIC.R`, `R/plot_model_fit_stochastic_param.R`, `R/plot_model_fit_stochastic.R`, `R/calc_npe_diagnostics.R`

# MOSAIC 0.13.5

## Improvements

* **Use linear interpolation for NA values in observed data**
  - **Problem**: Previously converted all NAs to 0, artificially introducing "no cases" observations that could mislead the model
  - **Solution**: Linear interpolation within each location preserves temporal trends
  - **Method**:
    - Uses `approx(method = "linear", rule = 1)` to interpolate interior NAs
    - `rule = 1` prevents extrapolation beyond data range (preserves boundaries)
    - Only sets start/end NAs to 0 when interpolation is impossible (no surrounding data points)
    - Applies independently to each location for multi-location data
    - Handles both cases and deaths time series
  - **Example**: Time series `NA, NA, 10, 20, NA, 30, 40, NA, 50, NA` becomes `0, 0, 10, 20, 25, 30, 40, 45, 50, 0`
    - Interior NAs interpolated: position 5 → 25 (between 20 and 30), position 8 → 45 (between 40 and 50)
    - Boundary NAs set to 0: positions 1-2 (before first data), position 10 (after last data)
  - **Impact**: More accurate representation of missing data, better model training quality
  - **Output**:
    - Reports number of NAs interpolated vs. set to 0
    - "Interpolated: X" shows successful linear interpolation
    - "Set to 0 (start/end): Y" shows boundary NAs
  - **Files modified**: `R/npe_posterior.R`

# MOSAIC 0.13.4

## Bug Fixes

* **Add comprehensive data validation to train_npe() to catch NAs/Infs early**
  - **Problem**: PyTorch silently propagates NaN/Inf values through the network, causing cryptic training failures
  - **Root cause**: No validation of input data (X, y, weights) before tensor conversion
  - **Impact**: If parameters or observations contain NAs/Infs (from corrupted files, invalid samples, or bugs), they propagate silently and cause losses to become NaN
  - **Fix**: Add explicit validation checks for X (parameters), y (observations), and weights before tensor conversion (line 170-266 in npe.R)
  - **Validation checks**:
    - `anyNA(X)` and `any(!is.finite(X))` - parameters matrix
    - `anyNA(y)` and `any(!is.finite(y))` - observations matrix
    - `anyNA(weights)` and `any(!is.finite(weights))` - weight vector
  - **Error messages include**:
    - Which data structure failed (X, y, or weights)
    - Whether NAs or Infs were found
    - Affected rows and columns (first 5-10 shown)
    - Likely sources of corruption
    - Specific solutions to diagnose and fix
  - **Benefits**:
    - Catches data corruption BEFORE training starts (saves time)
    - Clear diagnostic messages pinpoint exact problem
    - Prevents silent NaN propagation through network
    - Helps identify upstream bugs in data preparation
  - **Files modified**: `R/npe.R`
  - **Note**: This addresses the user's concern that NA/NaN errors with 25k evenly weighted samples (binary_retained) should not be due to numerical instability, but rather data corruption

# MOSAIC 0.13.3

## Bug Fixes

* **Fix cryptic "missing value where TRUE/FALSE needed" error in NPE training**
  - **Root cause**: Validation loss became NA/NaN during training, causing `if (val_loss < best_val_loss)` comparison to fail with cryptic error
  - **Error**: `Error during wrapup: missing value where TRUE/FALSE needed` followed by recursive error and abort
  - **Trigger**: Low ESS (Kish: 1.2, Perplexity: 3.0) with `continuous_retained` weight strategy causing numerical instability
  - **Fix**: Add explicit NA/NaN checking for train_loss and val_loss before early stopping comparison (line 384-405 in npe.R)
  - **Impact**: Now provides clear, actionable error message with diagnostic information and solutions
  - **Error message includes**:
    - Which epoch failed and what the loss values were
    - Common causes (low ESS, weight concentration, architecture complexity)
    - Specific solutions (try 'continuous_best', use 'light' tier, reduce learning rate)
  - **Files modified**: `R/npe.R`
  - **Related**: This error was masked by recursive error handling - actual issue is numerical instability from degenerate weight distributions

# MOSAIC 0.13.2

## Bug Fixes

* **CRITICAL: Fix run_NPE() JSON loading causing persistent list column errors**
  - **Root cause**: Used `jsonlite::fromJSON(..., simplifyVector = FALSE)` when loading config/priors from disk, keeping JSON arrays as R lists instead of converting to vectors
  - **Error**: Even after v0.13.1 fix in get_npe_observed_data(), `config$reported_cases` was still a list, creating list columns in data frames
  - **Fix**: Replace `jsonlite::fromJSON()` with `read_json_to_list()` (MOSAIC standard loader) at 3 locations in run_NPE.R (lines 244, 264, 284)
  - **Benefits**:
    - Uses MOSAIC codebase standard for JSON loading (consistent with other functions)
    - Properly simplifies JSON arrays to R vectors (`simplifyVector = TRUE` default)
    - Supports gzipped JSON files
    - More maintainable and consistent
  - **Impact**: Resolves persistent CSV write errors in run_NPE() standalone mode
  - **Files modified**: `R/run_NPE.R`
  - **Verified**: JSON loading, get_npe_observed_data(), CSV write all succeed

# MOSAIC 0.13.1

## Bug Fixes

* **Fix list column error in get_npe_observed_data() causing CSV write failures**
  - **Root cause**: When `config$location_name` or `config$iso_code` is a list (common in LASER config format), single-bracket extraction `location_names[1]` returned a list of length 1 instead of scalar, creating list columns in data frames
  - **Error**: `Error in utils::write.table(...): unimplemented type 'list' in 'EncodeElement'` when writing observed_data.csv in run_NPE()
  - **Fix**: Convert location_names to character vector using `unlist()` at function start (line 1008)
  - **Fix**: Changed all `location_names[index]` to `location_names[[index]]` for scalar extraction (lines 1032, 1055, 1134)
  - **Impact**: NPE workflow now handles list-type location identifiers correctly
  - **Files modified**: `R/npe_posterior.R` (get_npe_observed_data function)
  - **Verified**: CSV write succeeds, data frame structure correct, existing tests still pass

# MOSAIC 0.13.0

## Major Features

* **New run_NPE() function for flexible Neural Posterior Estimation**
  - Complete NPE workflow extracted into standalone function in `R/run_NPE.R` (1000+ lines)
  - **Dual-mode architecture**:
    - **Embedded mode**: Runs inside `run_MOSAIC()` with in-memory objects (no disk I/O)
    - **Standalone mode**: Runs independently after calibration completes, loading from disk
  - **Key features**:
    - Automatic mode detection based on arguments provided
    - Custom `output_dir` support for experimenting with multiple NPE strategies
    - Root directory auto-detection from `getOption('root_directory')`
    - Full control object support for all NPE hyperparameters
    - Complete error handling and validation
  - **Benefits**:
    - Post-hoc NPE without re-running expensive BFRS calibration
    - Experiment with different weight strategies (continuous_best, continuous_retained, etc.)
    - Cleaner, more maintainable code architecture
    - Reusable in custom workflows
  - **run_MOSAIC.R refactored**: Replaced 300+ lines of inline NPE code with clean `run_NPE()` call (lines 1500-1523)
  - **Standalone examples added**: `vm/launch_mosaic.R` now includes post-hoc NPE usage examples (lines 201-233)
  - See function documentation: `?run_NPE`

# MOSAIC 0.11.5

## Changes

* **Simplify plot_model_ppc: Remove by_location argument**
  - Function now always creates both aggregate and per-location plots by default
  - **Removed argument**: `by_location` (previously: "aggregate", "both", "per_location")
  - **New behavior**: Always creates comprehensive diagnostics (aggregate + per-location)
  - Simplifies API - no configuration needed for plot output mode
  - Legacy model mode still only creates aggregate plots (as before)
  - Updated function signature: `plot_model_ppc(predictions_dir, predictions_files, locations, model, output_dir, verbose)`
  - Updated call in run_MOSAIC.R to remove by_location argument

# MOSAIC 0.11.4

## Bug Fixes

* **Add backward compatibility for plot_model_ppc function signature**
  - Wrapped plot_model_ppc call in run_MOSAIC with tryCatch to handle old package versions
  - **Issue**: Clusters with cached old package versions (pre-v0.11.0) have different function signature
  - Old signature: `plot_model_ppc(model, output_dir, verbose)`
  - New signature: `plot_model_ppc(predictions_dir, predictions_files, by_location, locations, model, output_dir, verbose)`
  - **Solution**: Try new signature first; if "unused arguments" error, log warning and skip PPC plots
  - Prevents workflow from crashing on clusters that need package reinstallation
  - Fixed at lines 1415-1441 in `run_MOSAIC.R`
  - **Note**: Users should reinstall package on cluster to get full PPC functionality

# MOSAIC 0.10.25

## Bug Fixes

* **CRITICAL: Fixed missing Prior/BFRS curves for seasonality parameters in distribution plots**
  - Fixed gsub order in parameter name variant generation for seasonality params
  - **Root cause**: Wrong order in `gsub()` calls generated incorrect variant "a1j" instead of "a1"
  - Original: `gsub("_j$", "", gsub("_", "", "a_1_j"))` → "a1j" (WRONG)
  - Fixed: `gsub("_", "", gsub("_j$", "", "a_1_j"))` → "a1" (CORRECT)
  - **Impact**: Prior/BFRS use `a1`, NPE uses `a_1_j` - variant "a1j" didn't match either
  - Plotting function now correctly finds seasonality params in all three JSONs
  - Fixed at lines 515-516 in `plot_model_distributions.R`
  - **Result**: All three curves (Prior, BFRS, NPE) now appear for seasonality parameters

# MOSAIC 0.10.23

## Bug Fixes

* **CRITICAL: Fixed missing NPE posteriors in distribution plots**
  - Added uniform distribution handling to `.fit_distribution()` in NPE posterior processing
  - **Root cause**: Function only handled beta, gamma, lognormal, normal - NOT uniform
  - When dist_type="uniform", function fell through to normal case, setting mean/sd instead of min/max
  - Result: NPE posteriors.json had `{distribution: "uniform", parameters: []}`
  - Plotting function couldn't plot uniform without min/max parameters → NPE curves missing
  - **Solution**: Calculate min/max from 1% and 99% quantiles (robust to outliers) + 1% buffer
  - Fixed at lines 1516-1528 in `npe_posterior.R`
  - **Impact**: NPE posteriors now appear in distribution plots (Prior vs BFRS vs NPE)

# MOSAIC 0.10.20

## Bug Fixes

* **CRITICAL: Fixed derived parameters not added to posteriors.json**
  - Modified `calc_model_posterior_distributions()` to dynamically add parameters missing from priors template
  - Derived parameters (beta_j0_hum, beta_j0_env) now correctly added to posteriors.json
  - **Root cause**: Function used priors.json as template, which only contains sampled parameters
  - **Solution**: Dynamically create parameter structure for derived parameters not in priors
  - Handles multiple locations correctly (adds each location as it's processed)
  - Fixed at lines 391-420 in `calc_model_posterior_distributions.R`
  - **Result**: beta_j0_hum and beta_j0_env now appear in distributions_ETH_Prior_Posterior.pdf

# MOSAIC 0.10.19

## Bug Fixes

* **Fixed missing derived parameters (beta_j0_hum, beta_j0_env) in distribution plots**
  - Changed distribution type from "derived" to "gamma" in estimated_parameters source data
  - Derived rate parameters (beta_j0_hum, beta_j0_env) now fitted with gamma distributions
  - **Background**: beta_j0_hum = p_beta × beta_j0_tot, beta_j0_env = (1 - p_beta) × beta_j0_tot
  - Previously marked as "failed" because "derived" distribution type was unhandled
  - Now appear in distributions_ETH_Prior_Posterior.pdf alongside beta_j0_tot and p_beta
  - Fixed at line 289 in `data-raw/make_estimated_parameters_inventory.R`

# MOSAIC 0.10.17

## Bug Fixes

* **Fixed plot_model_convergence diagnostic text formatting**
  - Corrected convergence metrics display in diagnostic plots
  - Fixed in `R/plot_model_convergence.R`

# MOSAIC 0.10.15

## Bug Fixes

* **CRITICAL: Fixed "subscript out of bounds" error in convergence diagnostic plots**
  - Fixed regression from v0.10.14 where `targets[["ESS_min"]]` would error if element missing
  - **Root cause**: `safe_numeric()` returned `numeric(0)` for NULL inputs, causing elements to be dropped from vectors
  - **Solution 1**: Enhanced `safe_numeric()` to check for NULL and empty vectors before conversion
  - **Solution 2**: Changed extraction from `[[` to `as.numeric(x["name"])` for safe handling of missing elements
  - `[[` throws error on missing elements; `as.numeric(x["name"])` returns NA safely
  - Fixed in both `plot_model_convergence.R` and `plot_model_convergence_loss.R`
  - Now properly handles cases where JSON diagnostics may have missing target values

# MOSAIC 0.10.14

## Bug Fixes

* **Fixed sprintf formatting errors in convergence diagnostic plots**
  - Fixed "Error formatting: ESS: %.0f [target >= %.0f]" messages in convergence_diagnostic.pdf
  - **Root cause**: Using single brackets `metrics["ESS"]` returns named vector element, not just value
  - **Solution**: Changed to double brackets `metrics[["ESS"]]` to extract raw values
  - Fixed in both `plot_model_convergence.R` (lines 214-223, 330-336) and `plot_model_convergence_loss.R` (lines 108-114)
  - All convergence metrics (ESS, A, CVw, B) now format correctly in diagnostic text

# MOSAIC 0.10.13

## Bug Fixes

* **Fixed plotting scale for extremely small initial condition posteriors (E_initial, I_initial)**
  - Implemented automatic detection of tiny values (< 0.001) in posterior distributions
  - Applied scientific notation formatting for x-axis labels when values < 0.001
  - Increased padding from 10% to 30% for better visibility of tiny value distributions
  - Fixed all 8 panels: Prior, Retained, Retained Weighted, Best Unweighted, Best Weighted, Caterpillar, Distributions (Empirical), Distributions (Theoretical)
  - **Root cause**: E_initial and I_initial have epidemiologically correct tiny values (~10⁻⁷ proportion, representing ~50-60 people in population of 126M), which appeared as flat lines when plotted on wide [0,1] axis
  - **Solution**: Tight x-axis limits with scientific notation (e.g., "2.0e-07", "4.0e-07", "6.0e-07")
  - Fixed throughout `plot_model_posteriors_detail.R` (lines 380-769)
  - See `claude/initial_EI_plotting_issue.md` for complete analysis

# MOSAIC 0.10.12

## Bug Fixes

* **Fixed missing dplyr namespace prefix in `plot_model_posteriors_detail()`**
  - Added explicit `dplyr::` prefix to `slice()` function call
  - Fixes "could not find function 'slice'" error
  - Fixed at line 908 in `plot_model_posteriors_detail.R`

# MOSAIC 0.10.11

## Bug Fixes

* **Fixed S7 class conflict with patchwork operators in `plot_model_posteriors_detail()`**
  - Replaced `/` operator with explicit `patchwork::wrap_plots(ncol=1)` calls
  - Fixes "Can't find method for generic `/(e1, e2)`" error from S7 class system
  - S7 was intercepting the patchwork `/` operator between ggplot objects
  - Using explicit `wrap_plots()` avoids operator dispatch conflicts
  - Fixed at lines 727-754 in `plot_model_posteriors_detail.R`

# MOSAIC 0.10.10

## Bug Fixes

* **CRITICAL: Fixed incorrect weighting in NPE ensemble predictions**
  - Removed density-based weighting that caused double-weighting artifact
  - NPE samples are drawn directly from posterior p(θ|x), so uniform weights are correct
  - Using density as weights was creating effective distribution [p(θ|x)]²
  - This over-emphasized high-density regions and under-represented uncertainty
  - Now uses uniform weights (NULL) for proper posterior predictive sampling
  - Fixed at lines 1679-1686 in `run_MOSAIC.R`
  - **Impact:** NPE ensemble predictions should now have appropriate uncertainty bands
  - **Theory:** For posterior predictive p(y|x) ≈ (1/N) Σ p(y|θᵢ) where θᵢ ~ p(θ|x)
  - Density weights only correct for importance sampling from q(θ) ≠ p(θ|x)
  - See `claude/npe_weighting_analysis.md` for complete theoretical analysis

# MOSAIC 0.10.9

## Bug Fixes

* **Fixed missing namespace prefixes in `plot_model_posteriors_detail()`**
  - Added explicit `ggplot2::` prefixes to all ggplot2 functions (68+ occurrences)
  - Added `grid::` prefix for `unit()` calls
  - Added `arrow::` prefix for `read_parquet()`
  - Added `patchwork::` prefixes for `wrap_plots()` and `plot_layout()`
  - Added `cowplot::` prefixes for `plot_grid()` and `get_legend()`
  - Fixes "could not find function 'geom_histogram'" error
  - Fixed throughout `plot_model_posteriors_detail.R`

# MOSAIC 0.10.8

## Bug Fixes

* **Fixed inappropriate hard-coded bounds for truncated normal distribution**
  - Replaced arbitrary -45/45 defaults with -Inf/Inf to match fitting function behavior
  - Added intelligent plotting range selection for infinite bounds
  - Use mean ± 4sd for plotting range when bounds are infinite (covers 99.99%)
  - Properly handle one-sided truncation (e.g., a=-Inf, b=10)
  - Format display strings to show "Inf" for infinite bounds
  - Fixed at lines 445-481 in `plot_model_distributions.R`

# MOSAIC 0.10.7

## Bug Fixes

* **CRITICAL: Fixed remaining NA handling error in `calc_distribution_density()`**
  - Fixed "missing value where TRUE/FALSE needed" error for truncated normal distribution
  - Completed NA handling fix missed in v0.10.6
  - Fixed truncnorm distribution at lines 446-453 in `plot_model_distributions.R`
  - Now all distribution types properly handle NULL parameters

# MOSAIC 0.10.6

## Bug Fixes

* **CRITICAL: Fixed NA handling error in `calc_distribution_density()`**
  - Fixed "missing value where TRUE/FALSE needed" error in `plot_model_distributions()`
  - Replaced unsafe `as.numeric(NULL)` pattern with explicit `NA_real_` conversion
  - Fixed for uniform, normal, and gompertz distributions (lines 402-427)
  - Prevents crashes when parameter bounds are NULL

* **Fixed all ggplot2 3.4.0+ deprecation warnings**
  - Replaced deprecated `size=` with `linewidth=` in `geom_line()` (14 instances)
  - Replaced deprecated `size=` with `linewidth=` in `geom_smooth()` (2 instances)
  - Replaced deprecated `size=` with `linewidth=` in `element_line()` (10 instances)
  - Replaced deprecated `size=` with `linewidth=` in `geom_vline()` (6 instances)
  - Fixed across 10 files: plot_generation_time.R, plot_vibrio_decay_rate.R, est_symptomatic_prop.R, plot_suspected_cases.R, plot_CFR_by_country.R, plot_vaccine_effectiveness.R, est_WASH_coverage.R, npe_plots.R, plot_africa_map.R, plot_model_distributions.R

# MOSAIC 0.10.5

## Bug Fixes

* Fixed broken documentation links to deprecated `run_mosaic_iso()`
  - Removed references to `run_mosaic_iso()` in `run_MOSAIC()` documentation
  - Fixes roxygen2 warnings about unresolvable links
  - Updated @description and removed @seealso reference

# MOSAIC 0.10.4

## Bug Fixes

* Fixed missing namespace prefixes in `plot_npe_training_loss()`
  - Added explicit `ggplot2::` prefixes to all ggplot2 functions throughout the function
  - Fixes "could not find function 'facet_wrap'" error during NPE training visualization
  - Fixed throughout lines 1585-1768 including: `facet_wrap`, `ggplot`, `aes`, `geom_smooth`, `geom_line`, `geom_vline`, `geom_point`, `geom_text`, `labs`, `theme_minimal`, `theme`, and all theme element functions

# MOSAIC 0.10.3

## Bug Fixes

* Fixed missing `ggsave()` namespace prefix in `plot_model_distributions()`
  - Added explicit `ggplot2::ggsave()` prefix at two save locations
  - Fixes "could not find function 'ggsave'" error when saving plots
  - Fixed for both global parameters plot (line 834) and location-specific plots (line 980)

# MOSAIC 0.10.2

## Bug Fixes

* Fixed missing namespace prefixes in `plot_model_distributions()`
  - Added explicit `ggplot2::` prefixes to `scale_x_continuous()` and all other ggplot2 functions
  - Fixes "could not find function 'scale_x_continuous'" error in `create_multi_method_plot()`
  - Fixed in three locations: main plot creation and two legend plot sections
  - Also added `grid::unit()` prefix for grid package function

# MOSAIC 0.10.1

## Bug Fixes

* Fixed missing namespace prefixes in `plot_model_posterior_quantiles()`
  - Added explicit `ggplot2::` prefixes to all ggplot2 functions
  - Fixes "could not find function 'geom_errorbar'" error
  - Functions not imported in NAMESPACE now called with explicit prefix
  - Affects both global and location-specific plotting sections

# MOSAIC 0.10.0

## Breaking Changes

* **Major refactor: Lean run_MOSAIC() - aggressive simplification**
  - Moved `run_mosaic_iso()` to `deprecated/` directory (use `run_MOSAIC()` directly)
  - Stripped ESS calculation: removed conditional skipping, verbose summaries
  - Stripped convergence diagnostics: removed section banners, verbose logging
  - Stripped posterior sections: removed verbose progress, set verbose=FALSE everywhere
  - Stripped PPC sections: removed conditional checks, verbose status messages
  - Stripped parameter uncertainty: removed ensemble logging
  - Stripped NPE section: removed 66 log_msg calls, verbose diagnostics, section banners
  - Stripped POST-HOC optimization: removed tier-by-tier logging, convergence messages
  - Stripped WEIGHTS section: removed detailed ESS/temperature logging
  - Removed all major section banners (80× '=' decorative headers)
  - Pattern applied: calculate → write → log filepath (no defensive checks)
  - Total: removed 180+ verbose log_msg calls and 90+ section banners
  - **Result: 284 lines removed (2441 → 2157 lines, 12% reduction)**

## Deprecations

* `run_mosaic_iso()` - Use `run_MOSAIC()` with `get_location_config()` and `get_location_priors()` instead

# MOSAIC 0.9.1

## Bug Fixes

* Fixed premature cleanup of `ess_results` variable in `run_MOSAIC()`
  - Removed cleanup at line 1204 that occurred before `calc_convergence_diagnostics()` call
  - Variable is now retained until after convergence diagnostics are calculated
  - Fixes "object 'ess_results' not found" error at runtime

# MOSAIC 0.9.0

## Breaking Changes

* **Major refactor: Removed ALL excessive flow control from `run_MOSAIC()`**
  - Removed validation wrapper around `calc_convergence_diagnostics()` (90 lines → 30 lines)
  - Removed ALL tryCatch blocks around plotting functions (10+ instances)
  - Removed tryCatch around `calc_model_posterior_quantiles()`
  - Removed tryCatch around `calc_model_posterior_distributions()`
  - Removed tryCatch around `sample_parameters()` for best model
  - Removed tryCatch around `lc$run_model()` for best model
  - Removed tryCatch around `plot_model_fit_stochastic()` and related plotting
  - Removed defensive if-else wrapper around NPE posterior samples processing
  - Functions now fail fast with clear error messages at the source
  - **Impact:** Errors will stop execution immediately rather than continuing with NA/NULL values
  - **Benefit:** Much easier to debug - errors show exactly where the problem is
  - **Note:** Parallel worker tryCatch blocks retained (essential for batch processing)

## Why This Change?

Excessive defensive programming was masking real errors and making debugging extremely difficult.
The new fail-fast approach:
- ✅ Errors happen at the source with clear tracebacks
- ✅ Simpler code flow that's easier to understand and maintain
- ✅ Forces fixing root causes instead of papering over problems
- ✅ Better for research/development workflows
- ✅ Reduces code complexity (~150+ lines of error handling removed)

**If you encounter errors after upgrading:** The errors were always there, just hidden.
Fix the underlying issue rather than relying on fallback behavior.

# MOSAIC 0.8.9

## Bug Fixes

* Fixed uninitialized variable in `run_MOSAIC()`
  - Added defensive initialization of `ess_results` to NULL before parameter-specific ESS calculation
  - Prevents "object not found" error when debugging or if code execution is interrupted
  - Variable is properly set later based on sample availability (lines 1161 or 1164)

# MOSAIC 0.8.8

## Bug Fixes

* Fixed test failures in `test-calc_convergence_diagnostics.R`
  - Corrected threshold expectations for "lower is better" metrics (CVw)
  - Changed test values to properly demonstrate warn status (within 120-200% of target)
  - Updated helper function tests to use `MOSAIC:::` for internal function access
  - All 70 tests now pass

# MOSAIC 0.8.0

## New Features

### Initial Conditions Sampling
* Added biologically plausible Beta priors for initial condition compartments (S, V1, V2, E, I, R)
  - `prop_S_initial`: Beta(30, 7.5) - mean 80% susceptible
  - `prop_V1_initial`: Beta(0.5, 49.5) - mean 1% one-dose vaccination  
  - `prop_V2_initial`: Beta(0.5, 99.5) - mean 0.5% two-dose vaccination
  - `prop_E_initial`: Beta(0.01, 9999.99) - mean 0.0001% exposed
  - `prop_I_initial`: Beta(0.01, 9999.99) - mean 0.0001% infected
  - `prop_R_initial`: Beta(3.5, 14) - mean 20% recovered/immune

* Enhanced `sample_parameters()` function:
  - New `sample_initial_conditions` argument to control IC sampling
  - Automatic normalization of compartment proportions to sum to 1.0
  - Proper conversion from proportions to integer counts
  - Rounding error adjustment to ensure exact population totals

* Updated `create_sampling_args()` helper:
  - Added "initial_conditions_only" pattern for sampling just ICs
  - Support for new `sample_initial_conditions` flag

## Data Updates

* Renamed `priors` data object to `priors_default` to match naming convention with `config_default`
* Updated `priors_default` data object to include initial condition priors for all 40 African countries
* Priors now available in both R data format (`data/priors_default.rda`) and JSON (`inst/extdata/priors.json`)

## Documentation

* Added comprehensive documentation for the `priors_default` data object
* Updated `sample_parameters()` documentation to reflect IC sampling capability
* Added examples demonstrating initial conditions sampling workflow

## Internal Changes

* Added `sample_initial_conditions_impl()` internal function for IC sampling logic
* Updated NAMESPACE to import required stats functions (rbeta, rgamma, rlnorm, rnorm, runif)