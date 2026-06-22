---
name: 2015-window-runtime-blast
description: Runtime blast radius of a long (2015->present, ~4320-col) MOSAIC config; the real OOM cliff is calc_model_ensemble dense 4-D arrays, not per-worker engine state
metadata:
  type: project
---

A back-history (2015 `date_start`) config makes every `*_jt` + reported matrix ~3.1x wider
(~4320 vs ~1398 daily cols). Red-team of the v0.46.2/v0.46.3 build-start-date generalization.

**Verdict on the env-var contract (`MOSAIC_BUILD_DATE_START`):** robust against the leakage angle.
Nothing in the package `Sys.setenv`s it; `date_start` is NEVER read from the env at runtime — only
from the baked-in `config$date_start`. So a build session that left it set cannot poison a run, and
the window is fully baked into `config_default`. The genuine fragility is BUILD-TIME desync: an
explicit function arg would be safer than env+install-order, because the make_config fallback
("2023-01-01") and make_priors fallback (installed `config_default$date_start`) can silently
disagree if the documented export->priors->install->config order is skipped.

**Runtime memory — the engine/per-worker concern is a NON-issue.** laser-cholera state arrays are
`(nticks+1, npatches)` where `nticks=(date_stop-date_start).days+1`; ~25-30 such arrays => ~17 MB
at 4320 cols (vs ~6 MB at 1398). The documented 2GB/worker budget is dominated by numba JIT +
interpreter + reticulate, NOT the arrays. 3x widening does NOT threaten the 16-core=32GB rule on the
calibration (sampling) path. config_default itself goes ~4.5 MB -> ~14 MB (10 full-width matrices).

**The REAL OOM cliff is the post-calibration ENSEMBLE path, `R/calc_model_ensemble.R`.** It allocates
TWO dense 4-D arrays `cases_array`/`deaths_array` = `[n_loc x n_time x n_param_sets x n_stoch]`
(lines 281-282) on the SINGLE orchestrator process, AND in the local path holds the full gathered
`results_list` simultaneously during the fill loop (lines 383-402). With production defaults
(`max_best_subset=1000`, `n_iter_ensemble=10`) at 2015 width: arrays alone ~27.6 GB + gathered list
~14 GB => 40+ GB peak on one node. At 2023 width the same is ~8.9 GB arrays (fine). So a 2015 build
calibrates fine but can OOM at the ensemble step unless `max_best_subset`/`n_iter_ensemble` are cut.

**Things that are correctly window-agnostic (verified, no fix needed):**
- Engine `nticks=(date_stop-date_start).days+1`; all `*_jt` asserted `(nticks, npatches)`. Build's
  `t <- seq.Date(date_start,date_stop,by="day")` matches it exactly.
- laser issue #82 boundary is a FIXED +1 (analyzer clamps `nreports=min(obs.shape[1], incidence-1)`)
  — NOT amplified by window length. The MOSAIC artifact mask (`.mosaic_mask_central_for_scoring`)
  masks by COLUMN relative to `nc` (2 cases-warmup + 1 final-deaths) — fixed count, negligible at 4320.
- calc_model_likelihood / R2 / ESS / WIS are all O(n_loc x T) linear (single location pass, vectorized
  over time; WIS sorts a fixed 5 quantiles). 3x T = 3x time, no O(T^2) cliff. `weights_time` resolved
  from `ncol(config$reported_cases)` at runtime (run_MOSAIC.R:1106/1273) — window-adaptive.
- `fit_windows` default `c(365,120,90,60,30)` are last-N-days diagnostic R2 windows, not a fit/forecast
  split — length-agnostic.

**Should-fix cosmetics:** `plot_model_ensemble.R:268` hardcodes `scale_x_date(date_breaks="3 months")`
=> ~44 ticks over 11 years (unreadable). `save_simresults=TRUE` (default FALSE, Dask-path hard-errors)
long-format blast ~423 MB/iter in RAM at 4320 cols + replicates ~300 param cols across 172,800 rows/sim.

Related: [[project_optimize_subset_levers]] (the subset that feeds n_param_sets here).
