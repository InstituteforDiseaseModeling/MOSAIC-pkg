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

## Best/medioid prediction plots: independent `n_iter` and parallel execution

Best and medioid single-config prediction plots previously reused `ensemble_n_sims_per_param` (default 5, sequential) because they were refactored into `calc_model_ensemble()` in 0.29.2 without giving them their own iteration control. For tight stochastic CI envelopes on the best/medioid plots, users had to bump the ensemble count — paying the cost across all N posterior parameter sets.

This release splits the two:

* `control$predictions$n_iter_ensemble` (default **10L**) — stochastic runs per posterior parameter set in the weighted ensemble. Renamed from `ensemble_n_sims_per_param`.
* `control$predictions$n_iter_best` (default **100L**) — stochastic runs for the best and medioid single-config plots. Applied identically to both models.

Best/medioid now also run in parallel: `calc_model_ensemble()` is invoked with `parallel = control$parallel$enable` and `n_cores = control$parallel$n_cores` (previously hardcoded `parallel = FALSE`). Parallelization uses an internal PSOCK cluster — same infrastructure the posterior ensemble already uses in the non-Dask path. For Dask calibrations, the best/medioid step still uses local PSOCK since the Dask cluster is closed after the posterior-ensemble sims.

**Breaking rename:** `control$predictions$ensemble_n_sims_per_param` → `control$predictions$n_iter_ensemble`. User scripts (`vm/`, `azure/`, vignettes) updated accordingly. Existing scripts setting `best_model_n_sims` (previously an orphaned no-op control field) migrate to `n_iter_best` and are now wired in.

---

# MOSAIC 0.29.3

## Unified stochastic-median R² and bias across best, medioid, and ensemble

Follow-up to 0.29.2. The 0.29.2 fix routed the best and medioid prediction **plots** through `calc_model_ensemble()` + `plot_model_ensemble()` so the plot captions report R²/bias from the stochastic median. But the separate `"Best model R²"` / `"Medioid model R²"` **log lines** still came from a single deterministic `lc$run_model()` call, producing two different numbers for the same thing (log vs plot caption). Red-team review flagged the inconsistency.

This release eliminates the separate deterministic LASER calls for best/medioid and sources all three sets of reported metrics — best, medioid, ensemble — from the stochastic median of their respective `mosaic_ensemble` objects:

* `R/run_MOSAIC.R` best-model block reordered: `calc_model_ensemble(configs = list(config_best), ...)` is called once; R²/bias are computed from `best_ensemble$cases_median` / `$deaths_median` and passed to both the log line and downstream `summary.json`. Same refactor applied to the medioid block.
* Log format now: `"Best model R² (1 params x N stoch): cases = X (bias=Y), deaths = ..."` — mirrors the existing ensemble log format.
* Removes two per-run LASER calls (the single deterministic `best_model <-` and `medioid_model <-` runs) that are no longer needed; best/medioid each now run `n_ensemble_stochastic_per` LASER sims total (default 10), same as before the 0.29.2 fix *plus* plot but minus the deterministic R² helper run.
* Retires the `lc <- reticulate::import(...)` import in the main `run_MOSAIC` body — `calc_model_ensemble` handles the import internally.

No public API changes.

---

# MOSAIC 0.29.2

## Fix best/medioid prediction plots: `reported_cases`, stochastic CI, unified naming

`plot_model_fit()` (used by the best-model and medioid-model plots in `run_MOSAIC()`) rendered `model$results$expected_cases` — the back-calculated burden `new_symptomatic / rho`, typically 5-10x inflated vs surveillance-comparable cases. Every other part of the pipeline (likelihood, R², ensemble) used `model$results$reported_cases = Isym * rho / chi_eff`. The v0.14.22 commit that renamed `expected_cases` → `reported_cases` in sibling plotting functions missed this fourth file; the bug was latent until v0.22.15 re-wired `plot_model_fit()` into the best-model block, and real from v0.22.15 through v0.29.1.

**Fix consolidates best/medioid plots into the existing `calc_model_ensemble` + `plot_model_ensemble` pipeline** — the same functions the posterior ensemble uses — in single-config mode (one parameter set × N stochastic reruns). One codepath for all three plot types eliminates the parallel-implementation drift that caused the original bug.

**Changes:**

* `R/plot_model_ensemble.R`: new `file_prefix` (default `"ensemble"`) and `title_label` (default `"Posterior Ensemble"`) parameters; hardcoded filename and title strings replaced; single-param-set subtitle branch added.
* `R/run_MOSAIC.R`: best and medioid plot calls replaced with `calc_model_ensemble(configs = list(config_<...>), n_simulations_per_config = n_ensemble_stochastic_per, envelope_quantiles = c(0.025, 0.975))` + `plot_model_ensemble(file_prefix = "best" | "medioid", ...)`. Medioid output moved from `2_calibration/best_model/` to `3_results/figures/predictions/` alongside the ensemble plots. Explicit `file_prefix = "ensemble"` added at the main ensemble plot call for self-documentation.
* Prediction-CSV combining loop extended to iterate `c("ensemble", "best", "medioid", "stochastic")` and filename pattern renamed from `all_predictions_<type>.csv` → `predictions_<type>_all.csv` for consistency with the plot naming.
* `R/plot_model_fit.R`: deleted (retired). The function was the single source of the drift bug and had no internal callers after the refactor.

**Output naming** (all under `3_results/figures/predictions/` unless noted):

* `predictions_<prefix>_<LOC>.pdf` + `.csv` per-location (prefixes: `ensemble`, `best`, `medioid`)
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

# MOSAIC (development version)

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