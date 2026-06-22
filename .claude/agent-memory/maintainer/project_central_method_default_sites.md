---
name: central-method-default-sites
description: The full sibling set of central_method default sites that must move in lockstep when the package default flips (mean<->median)
metadata:
  type: project
---

`central_method` (ensemble central tendency) has its package default defined/documented at
SEVEN sites that must change in lockstep when the default flips. v0.38 set them to "mean";
v0.46.1 reverted to "median". The authoritative set:

1. `R/run_MOSAIC_helpers.R` `.mosaic_resolve_central_method()` — the ULTIMATE default
   (NULL/empty + per-channel fallback). `rep("<default>", 2L)`.
2. `R/run_MOSAIC.R` `mosaic_control_defaults()$predictions$central_method` (+ inline comment).
3. `R/run_MOSAIC.R` `run_MOSAIC` @param roxygen for control$predictions$central_method.
4. `R/plot_model_ensemble.R` arg default + @param.
5. `R/run_rolling_cv.R` `run_rolling_cv()` arg default + @param, AND the two internals
   `.rcv_compile_all_models()` / `.rolling_cv_compile_run()` defaults.
6. `R/run_MOSAIC_infrastructure.R` r2_cases_ensemble @param doc.
7. `R/optimize_ensemble_subset.R` arg default (formal at ~line 128). NOTE: this one was
   ALREADY "median" before v0.46.1 and was NOT touched by the flip commit. Its roxygen
   (lines ~66-73) still calls "mean" the package default and says run_MOSAIC passes "mean"
   explicitly — STALE/WRONG as of v0.46.1 (run_MOSAIC passes the resolved control value,
   default median). Flag on any central_method audit.

Intentionally LEFT as "mean" (do NOT flip):
- `R/run_rolling_cv.R:428` `compile_rolling_cv_predictions()` `man$spec$central_method %||% "mean"`
  legacy-manifest fallback — old manifests were generated under the then-default mean; faithful
  to how that data was produced.
- `R/evaluate_rolling_cv.R` / `R/plot_rolling_cv.R` default-to-median-for-pre-central_method-parquets
  (these default to median for back-compat, unrelated to the package default).

Pinning test: `tests/testthat/test-central_method.R` "package default central tendency is median"
asserts sites 1,2,4,5 (resolver NULL, control defaults, plot_model_ensemble + run_rolling_cv
formals). It does NOT pin optimize_ensemble_subset's formal — a coverage gap for site 7.

Why: see [[project_central_method_v038]] for the mean-vs-median bias semantics (mean unmasks
implied-CFR bias by design; median lowers headline bias metrics).
How to apply: when reviewing any future central_method default change, grep `grep -rn 'central_method' R/`
and confirm ALL of sites 1-7 moved and the two intentional-mean fallbacks did NOT.
