---
name: plots-gated-data-writes
description: Full inventory of data artifacts (CSVs/objects) computed-or-written only under if(control$paths$plots) in run_MOSAIC tail + plot_* fns — the real surface for any viz/data-layer separation
metadata:
  type: project
---

When auditing the run_MOSAIC post-calibration tail (R/run_MOSAIC.R:2233-3190) for data
artifacts trapped behind `if(control$paths$plots)`, the COMPLETE set as of v0.50.0 is:

1. **Prediction CSVs** — written inside `plot_model_ensemble.R:288-298` (`save_predictions=TRUE`),
   called under plots-gate at run_MOSAIC.R:2941 (medoid) and 3091 (ensemble). Sole writer.
   Real current CSV header: `location,date,metric,observed,predicted_central,predicted_mean,
   predicted_median,central_method,ci_1_lower,ci_1_upper,ci_2_lower,ci_2_upper` — note `metric`
   (NOT "channel"), dynamic `ci_<k>_lower/upper` pairs (NOT single ci_lo/ci_hi), and NO `n_members`.
2. **parameter_sensitivity.csv** — HSIC compute is interleaved with the plot in
   plot_model_parameter_sensitivity.R:197-249, written at :349-350; called under plots-gate at
   run_MOSAIC.R:2740.
3. **convergence_status.csv** — derived metric/description/value/target/status table assembled
   INSIDE plot_model_convergence_status.R:414-423, called under plots-gate at run_MOSAIC.R:2235.
   This is a SECOND compute-trapped-in-plotter CSV, distinct from the already-unconditional
   `convergence_results.parquet`. Easy to miss.

**Plotters that write NO data (figure-only):** plot_model_likelihood, _posterior_quantiles,
_distributions, _posteriors_detail, _parameter_correlation, _psi_star_diagnostic, _ppc,
_subset_optimization.

**Always-unconditional data writes (safe):** samples.parquet (schema confirmed: sim,iter,seed_sim,
seed_iter,likelihood,<params> at run_MOSAIC.R:350), ensemble_candidate.rds (2447),
ensemble_optimized.rds (2547), posterior_quantiles.csv, posteriors.json (calc_model_posterior_
distributions at 2704 — UNGATED), convergence_results.parquet, model_fit_windows.csv (3066),
optimization_diagnostics.csv (2576), parameter_estimates.csv (3176), summary.json.

**Unserialized in-memory objects:** medoid_ensemble (built 2901, used 2941, never saved) and
subset_opt (whole object passed to plot at 2584; only $evaluation_table+$ensemble_optimized persisted).

**Combine loop (3117-3139):** NOT plots-gated itself, but reads back per-loc prediction CSVs, so
no-ops under plots=FALSE. pred_type "stochastic" in the list is a DEAD prefix — no writer exists in
the codebase (also referenced in plot_model_ppc.R:116). Only "ensemble"/"medoid" are written.

**No worker writes predictions:** Dask + PSOCK worker paths (run_MOSAIC_helpers.R) emit zero
prediction CSVs / plot_model_ensemble calls — prediction assembly lives only in the single tail.
So a #12-style Dask/PSOCK divergence does NOT threaten prediction-CSV decoupling.

**Masking args:** the medoid (2941) and ensemble (3091) plot calls pass NO masking args — they rely
on plot_model_ensemble defaults (mask_final_deaths_step=TRUE, n_cases_warmup_mask=2L,
score_idx_*=NULL→read from ensemble$artifact_mask). Any extracted assembly helper must replicate
these exact defaults or the CSVs change.

**Test coupling:** tests/testthat/test-plot_model_ensemble_boundary_mask.R and test-central_method.R
call plot_model_ensemble(save_predictions=TRUE) and read the plotter-written CSV. Deprecating
save_predictions to a no-op breaks these — they must be migrated to the new assembly helper.
test-run_MOSAIC_integration.R already runs run_MOSAIC(plots=FALSE) but needs engine+root (SKIPS on
CI per [[project_engine_tests_ci_gap]]) — a plots=FALSE data-parity guard inherits the same skip
unless built engine-free.
