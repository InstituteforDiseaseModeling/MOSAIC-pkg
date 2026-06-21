---
name: scoring-window-burnin
description: Per-channel scored-window (burn-in + deaths-era start) feature — slice-not-downweight, where the hot-loop precompute lives, and the local/Dask scoring divergence
metadata:
  type: project
---

Per-channel scoring window (run-time only via `control$likelihood`): omit the
unscored head from scoring without changing the sim. Knobs: `burn_in_days`
(default 0L), `deaths_score_start` (NULL or "YYYY-MM-DD"), `score_start_cases`.
All-default => idx_cases=idx_deaths=1 => NO slicing => bit-identical scoring.

**Why slice, not weights_time=0:** the shape terms in `calc_model_likelihood`
(peak timing/magnitude `.calc_peak_*_from_indices`, cumulative
`.ll_cumulative_progressive_nb`) do NOT honor `weights_time` — they scan/sum the
raw series. So the IC-spike head MUST be removed by slicing obs/est arrays.

**Architecture:**
- Resolver `.mosaic_resolve_score_window(config, control)` in
  run_MOSAIC_helpers.R returns `list(idx_cases, idx_deaths, n_time)`.
- Resolved ONCE at run_MOSAIC.R ~L1189 (CLUSTER INITIALISATION, before the
  use_dask branch) into private slot `control$likelihood$.score_window_resolved`,
  parallel to `.weights_time_resolved`. NOT serialized to control.json (resolution
  happens AFTER inputs are written at ~L1016), so the resume guard byte-compares
  only the PUBLIC knobs — resume-guarded for free.
- Local PSOCK worker (`.mosaic_run_simulation_worker`): precompute `s =
  min(idx_cases,idx_deaths)`, `.keep`, sliced `date_start` (config$date_start +
  (s-1) so the peak date_seq length matches sliced n_time_steps — else the L145
  length check fails over to WEEKLY and mis-snaps peaks), and the deaths-prefix
  weight clone ONCE outside the per-sim `for(j)` loop (hot path). In-loop: slice
  obs/est to `.keep`, pass sliced `.config_lik$date_start` + `.wt_lik` +
  deaths-prefix-zeroed `.wobs_deaths_lik`.
- R²/bias: `.mosaic_mask_central_for_scoring` generalized to NA columns
  `< score_idx_{cases,deaths}` per channel; carried in `ens$artifact_mask$
  score_idx_*` (calc_model_ensemble new params, default 1L). All R²/bias sites
  already route through that helper, so plumbing only.

**Local vs Dask scoring divergence (KNOWN, accepted):** the engine Python
`calc_model_likelihood` does NOT accept `weights_obs_cases/deaths` (MOSAIC-R-only
per-cell confidence weights). Dask scoring is therefore ALREADY unweighted on
per-cell weights (Lesson #12 note: broadcast for future lockstep, inert today).
For the scored window, the Dask worker (`mosaic_dask_worker.py
::_score_window_likelihood`) recomputes the LL on sliced arrays ONLY when
score_idx_*>1 (else keeps `model.log_likelihood`, bit-identical to the analyzer).
It mirrors the deaths residual-prefix zeroing by setting `obs_deaths[:,:prefix] =
np.nan` (engine NB masks non-finite obs; nan-aware shape terms ignore them)
rather than a per-cell weight (unsupported). Minor divergence: the default-OFF
deaths cumulative term sums prefix obs on the local path (weight zeroed, not the
value) but skips them on Dask (NaN'd) — irrelevant while cumulative weight=0.

**Files:** R/run_MOSAIC.R (defaults ~L3536, resolve ~L1189, inject
`.mosaic_inject_likelihood_settings`, worker precompute+slice, 2 calc_model_
ensemble calls), R/run_MOSAIC_helpers.R (resolver + mask + `.extract_base_config`
keep-list), R/calc_model_ensemble.R (params+artifact_mask), R/plot_model_
ensemble.R (score_idx_* NULL=>inherit ens$artifact_mask, blank head display).
Tests: test-burn_in_scoring_parity.R (new), test-dask_worker_schema_parity.R
(score_idx inject+broadcast), test-calc_model_ensemble.R (artifact_mask 4 fields).

Related: [[2015_window_runtime_blast]] (env-var contract robust at runtime).
