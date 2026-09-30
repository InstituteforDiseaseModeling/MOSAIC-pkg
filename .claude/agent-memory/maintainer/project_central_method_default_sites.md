---
name: central-method-default-sites
description: The full sibling set of central_method default sites that must move in lockstep when the package default flips (mean<->median); flipped back to mean in v0.98.0
metadata:
  type: project
---

`central_method` (ensemble central tendency) has its package default defined/documented at
the sites below, which must change in lockstep when the default flips. History: v0.38.0 set
"mean"; v0.46.1 reverted to "median"; **v0.98.0 set "mean" again** (CFR-v2.1 branch: the daily
median of sparse deaths reads 0 on most days, and the v2.1 deaths level is fitted, so the mean no
longer unmasks a ~2x implied-CFR bias).

1. `R/run_MOSAIC_helpers.R` `.mosaic_resolve_central_method()` -- the ULTIMATE default
   (NULL/empty + per-channel fallback). `rep("<default>", 2L)` + roxygen.
2. `R/run_MOSAIC.R` `mosaic_control_defaults()$predictions$central_method` (+ inline comment
   and the medoid-target comments).
3. `R/run_MOSAIC.R` roxygen item for control$predictions$central_method (renders into
   `man/mosaic_control_defaults.Rd`).
4. `R/plot_model_ensemble.R` `plot_model_ensemble()` AND the internal
   `.mosaic_assemble_prediction_table()` arg defaults + @param.
5. `R/run_rolling_cv.R` `run_rolling_cv()` arg default + @param, AND the two internals
   `.rcv_compile_all_models()` / `.rolling_cv_compile_run()` defaults.
6. `R/run_MOSAIC_infrastructure.R` r2_cases_ensemble @param doc.
7. `R/calc_model_ensemble.R` `.mosaic_build_trajectories()` internal default.
8. `R/calc_Reff.R` `.mosaic_reff_resim_ci()` `cases_central_method` default (the R_eff medoid
   target; `add_reproductive_numbers()` passes the run's value).

Deliberately NOT the package default:
- `R/optimize_ensemble_subset.R` formal stays "median" (Tier-2 bit-for-bit parity for direct
  calls); run_MOSAIC passes the resolved control value. Only its roxygen names the package default.
- `R/run_rolling_cv.R` `compile_rolling_cv_predictions()` `man$spec$central_method %||% "mean"`
  legacy-manifest fallback (old manifests were generated under the then-default mean).
- `R/evaluate_rolling_cv.R` / `R/plot_rolling_cv.R` default-to-median for pre-central_method
  parquets.
- Readers of COMPLETED runs whose control.json lacks the field (pre-v0.38.0 runs, which used the
  median): `render_MOSAIC_figures()` `.resolve_central()` and `add_reproductive_numbers()`
  (`%||% "median"`). A control.json that sets only ONE channel resolves the other to the
  CURRENT default, not the default of its era -- a known edge case.

Pinning test: `tests/testthat/test-central_method.R` "package default central tendency is mean
(v0.98.0)" asserts sites 1, 2, 4 (both), 5 (all three), 7, 8 and the optimizer's median formal.

How to apply: when reviewing any future central_method default change, grep
`grep -rn 'central_method' R/` and confirm every site above moved and the deliberate exceptions
did not. See [[project_central_method_v038]] for the mean-vs-median semantics.
