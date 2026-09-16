---
name: forecast-cv-ocv4-redteam
description: Stat red-team of the OCV-4 quarterly forecast-CV run (claude/forecast_cv_ocv4_q2yr_run.R) — two ESS concepts, missing moving-block bootstrap, nb_k_min circularity, coverage-vs-median
metadata:
  type: project
---

Red-teamed `claude/forecast_cv_ocv4_q2yr_run.R` + `PLAN_OCV4_RUN.md` (2026-07-06, MOSAIC v0.60.0).

**PREFLIGHT UPDATE (2026-07-07):** SPEC now `ESS_param=500` (was 1000), `max_simulations_total=80000`.
Confirmed mechanics for the accuracy-max audit: scored PIs (pi50/pi95) are WEIGHTED EMPIRICAL
QUANTILES over the pooled [n_best_subset x n_iter_ensemble] sample (calc_model_ensemble.R:1013-1019,
`weighted_quantiles(values, sim_weights, c(0.5, envelope_quantiles))`), NOT an NB band around the
median — so quantile precision is governed by the best-subset WEIGHT-ESS x n_iter_ensemble, and
n_iter_ensemble=10 is thin for the 95% tails. Best-subset weights: `pmin(dAIC,4.0)` + eta=0.5
(run_MOSAIC.R:2157) caps weight peakedness -> that truncation IS what keeps ess_best up. optimize_
objective drives optimize_ensemble_subset's minimand: "mae"=normalized point MAE, "wis"=normalized
WIS (Bracher, 0.5*MAE coeff present, .compute_wis_from_quantiles); ensemble_opt inherits it.
run_rolling_cv defaults optimize_subset=TRUE (overrides control FALSE) so ensemble_opt IS emitted.

**TWO DIFFERENT ESS CONCEPTS — the design pays for one, gates on another (correct but must be understood):**
- `targets$ESS_param=500/1000` = per-parameter MARGINAL importance ESS (KDE/binned over each param's
  posterior), the calibration STOPPING rule. Reached via `calc_model_ess_parameter`. n_grid scales
  as `100*(1+log(ESS/100))` → 230 at ESS=1000 (run_MOSAIC.R:1941). Convergence = ESS_param_prop
  (0.95) of params ≥ ESS_param.
- The scoring gate `ess_min=50` reads `metrics$ess_best$value` = `ESS_B_final` =
  `calc_model_ess(best_subset Akaike weights, perplexity)` (run_MOSAIC.R:2082, best subset
  truncated ΔAIC=4, eta=0.5). This is the IMPORTANCE-WEIGHT ESS of the retained best subset,
  NOT the per-parameter ESS. `.rcv_read_ess_best` (run_rolling_cv.R:704) pulls it into the `ess`
  col; `evaluate_rolling_cv` flags `ess_ok = ess>=ess_min`.
- They are DIFFERENT and NOT comparable in magnitude. ESS_best target is left at DEFAULT 100
  (SPEC only raises ESS_param). So gate=50 is ~half the best-subset ESS target — coherent, lax.

**`target_r2_adaptive=0.90` is NOT an in-sample data fit** — it is the R² of the
`lm(threshold_ess ~ sqrt(n_sims))` growth-curve regression (run_MOSAIC_helpers.R:1897), a pure
convergence stopping heuristic. Cannot overfit the IS window / cannot inflate OOS skill. Dispels
the "0.90 IS R² → optimistic OOS" worry.

**MISSING machinery — moving-block bootstrap does not exist.** PLAN §7 promises "moving-block
bootstrap (block=2)"; `.rcv_boot_mean` (evaluate_rolling_cv.R:389) is a plain IID cell resample
`sample(x, length(x), replace=TRUE)`. No block logic anywhere in R/ (grep confirmed). At n≈8-9
quarterly origins the CI is a small-n IID bootstrap regardless; `min_cells_ci=5` keeps it alive
(8-9 ≥ 5) so it WILL emit a CI that the plan's own text says shouldn't be trusted. Primary verdict
should be per-origin win/loss, not the bootstrap CI.

**nb_k_min_cases=10 is used in BOTH calibration objective AND is a validated bias lever** — raising
the NB dispersion floor (calc_log_likelihood_distributions.R:413) caps variance = mu + mu^2/k at a
tighter value, down-weighting high-count misfit → narrower effective predictive spread. Using it in
the LL that selects the posterior AND then scoring that posterior's WIS/coverage is defensible only
if disclosed: it is a fixed modeling choice, not tuned per-cutoff, so not leakage — but it
mechanically biases coverage. Prior run already showed cases PIs too NARROW; k_min=10 pushes the
same direction, so the design may MASK (not surface) cases under-coverage. Report cov50/cov95 per
channel and read them as k_min-conditional.

**central_method=median + NB dispersion:** coverage is still interpretable (PIs are empirical
envelope quantiles of the ensemble, not median±k), so cov50/cov95 remain valid calibration
diagnostics independent of the central point choice.

**Horizon nesting:** {1,2,3}mo are CUMULATIVE ≤h (evaluate_rolling_cv.R:159, wend=oos0+ceil(h*30.44));
3mo window ⊇ 1,2mo. The decay curve is NOT independent evidence — never present 1/2/3mo as 3
independent skill points.
