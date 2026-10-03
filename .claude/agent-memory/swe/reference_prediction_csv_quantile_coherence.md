---
name: prediction-csv-quantile-coherence
description: predictions_*.csv predicted_median must be the median of the SAME draws as ci_* (frozen v1.0 evaluator's M-OUTPUT BLOCK reads it); medoid_ensemble.rds carries only a 95% pair, so rolling-CV medoid rows are re-simulated, never read from it
metadata:
  type: reference
---

**The contract.** The frozen acceptance evaluator (laptop
`claude/deploy_v0100/acceptance_v1.0.1/evaluate_suite.R`, `.read_pred`) maps
`cen = predicted_central`, `med = predicted_median`, `l95/u95 = ci_1_*`, `l50/u50 = ci_2_*`.
Bias/R2 use `cen`; WIS and the M-OUTPUT nesting check (l95<=l50<=med<=u50<=u95 on >=99% of
EVAL days, a BLOCK => NO-GO) use `med`. So `predicted_median` must share draws with `ci_*`,
while `predicted_central` is the engine line.

**What broke (v0.101.0 RC, red team OBS-1).** `ci_bounds` became observation-level but
`.mosaic_assemble_prediction_table()` still wrote the engine median into `predicted_median`.
At small weekly NB k (KEN 0.135, GHA 0.143) the median sat above its own 50% band: 58 of 168
rehearsal (country, k) cells failed M-OUTPUT, min 0.500. Fixed on fix/v0101-rt-obs (10779dbb9):
`predicted_median <- ensemble$predictive_median[[chan]]` when `observation_model[[chan]]`.
After: 1.000 in all 168 cells. Probes: laptop `claude/v0101_rt_obs/moutput/`.

**How to apply:** any column a WIS/nesting consumer pairs with intervals must come from the
same draws; never derive a display fill of the *central* line from `predicted_median` (the
burn-in head fill now takes the engine median from a median assembly's `predicted_central`).

**medoid_ensemble.rds trap.** `run_MOSAIC` builds it with `envelope_quantiles = c(0.025, 0.975)`
(only a 95% pair) and `n_iter_best` reruns. `evaluate_rolling_cv()` WIS/cov50 need pi50 AND pi95,
so rolling-CV medoid rows taken from it would lose WIS. `.rcv_simulate_config()` instead re-runs
the config through `calc_model_ensemble()` with the template's quantiles, the template's recorded
`observation_model` (full-precision k; `nb_dispersion.csv` only has 15 digits) and the run's
`deaths_integration.rds` (gate it on `template$cfr_posterior`). Seeds 1001.. like the run medoid.

Related: [[render-figure-source-traps]], [[control-json-wrapper-trap]].
