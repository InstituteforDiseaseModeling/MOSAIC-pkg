# Post-Calibration Ensemble Sims: Option B — Second Dask Cluster

**Date**: 2026-04-04
**Branch**: `tt/azure-coiled`
**Status**: Implemented and validated

---

## Background

In v0.19.0, `run_MOSAIC_dask()` was merged into `run_MOSAIC()` via a `dask_spec` parameter. When `dask_spec` is provided, calibration simulations are dispatched to Coiled workers on Azure; when `NULL` (default), a local R parallel cluster is used.

The Coiled cluster is **intentionally closed before post-processing begins** (`run_MOSAIC.R` ~line 1214). This prevents Python event loop starvation and TLS heartbeat timeout errors during the R-heavy post-calibration phase (500+ lines of weight calculation, parquet I/O, convergence diagnostics, posterior estimation).

## Problem

The post-calibration phase includes two steps that require LASER simulations:

| Step | Function | Default sims | Previous parallelism |
|------|----------|-------------|---------------------|
| Stochastic ensemble R² + prediction plots | `calc_model_ensemble()` + `plot_model_ensemble()` | `best_model_n_sims` (default 100) | Local R parallel |
| Parameter uncertainty ensemble | `plot_model_fit_stochastic_param()` | posterior param sets × `ensemble_n_sims_per_param` each (up to ~10,000 total) | Local R parallel |

For a large Coiled run (e.g. 50,000 sims on 100 workers completing in ~30 min), the parameter uncertainty ensemble alone could run for **hours** on the local machine, defeating the benefit of cloud scaling. The orchestrator VM must stay alive and paying for the full duration.

## Solution: Fresh Cluster for Post-Calibration

Open a **second, smaller** Dask cluster specifically for post-calibration LASER sims, then close it before R-side analysis.

### Why not keep the calibration cluster alive?

The TLS timeout issue is caused by the cluster being **idle** while R performs heavy post-processing (500+ lines between cluster close and the point where post-cal sims are needed). Keeping the cluster alive through this phase risks the same heartbeat timeouts that motivated the original close.

### Why not restructure to run sims before cluster close?

The post-cal sims require `config_best` and posterior parameter sets, which are derived from the combined calibration results. Computing those requires loading and processing all calibration parquets — exactly the R-heavy work that causes TLS issues. Restructuring would be invasive and fragile.

### Flow

```
BEFORE (local post-cal):
  Calibration (Dask) → Close cluster → R post-processing → config_best
  → calc_model_ensemble (100 LASER, local)           ← SLOW
  → plot_model_fit_stochastic_param (10K LASER, local) ← VERY SLOW
  → Write summary

AFTER (Option B):
  Calibration (Dask) → Close cluster → R post-processing → config_best
  → Open post-cal cluster (smaller, ~20 workers)
  → Dispatch ensemble sims (5 ensemble, Dask)         ← FAST
  → Dispatch stochastic sims (80 stochastic, Dask)    ← FAST
  → Gather all results → Close post-cal cluster
  → calc_model_ensemble(precomputed_results = ...)     ← No LASER, just aggregation
  → plot_model_fit_stochastic_param(precomputed_results = ...) ← No LASER, just plotting
  → Write summary
```

## Implementation

### Files changed

| File | Change |
|------|--------|
| `inst/python/mosaic_dask_worker.py` | Added `run_laser_postca()` — lightweight worker for post-cal sims |
| `R/run_MOSAIC_helpers.R` | Added `.mosaic_postca_dask()` — opens cluster, dispatches, gathers, closes |
| `R/calc_model_ensemble.R` | New `precomputed_results` param; skips local LASER when provided |
| `R/plot_model_fit_stochastic_param.R` | Same `precomputed_results` pattern |
| `R/run_MOSAIC.R` | Added pre-dispatch block before ensemble/stochastic sections |

### Python worker: `run_laser_postca()`

Simpler than `run_laser_sim()` (used during calibration). Takes a complete config as JSON + scattered matrix fields and runs a single LASER iteration with a given seed. No parameter merging, no multi-iteration handling.

```python
def run_laser_postca(task_id, seed, config_json, extra_fields):
    # Returns: {task_id, seed, success, reported_cases, disease_deaths}
```

### R helper: `.mosaic_postca_dask()`

```r
.mosaic_postca_dask(
  dask_spec,          # Same spec as calibration
  config_best,        # Best-fit config (for ensemble sims)
  param_configs,      # List of posterior param configs (for stochastic sims)
  n_ensemble,         # Number of ensemble sims
  n_stochastic_per,   # Sims per param config
  log_msg             # Logging function
)
# Returns: list(ensemble_results, stochastic_results)
```

Key design choices:
- Uses **at most 20 workers** (post-cal needs fewer than calibration)
- **10-minute idle timeout** (short — just doing a quick burst of sims)
- **Scatters matrix fields separately** from JSON config (matrices are too large for JSON)
- For stochastic sims, each param config gets its own scattered matrix future (psi_jt differs per config due to `apply_psi_star_calibration()`)

### Precomputed results interface

Both `calc_model_ensemble()` and `plot_model_fit_stochastic_param()` accept an optional `precomputed_results` parameter:

**For `calc_model_ensemble`**: list of `list(reported_cases = matrix, disease_deaths = matrix, success = TRUE, seed = int)`

**For `plot_model_fit_stochastic_param`**: list of `list(param_idx = int, stoch_idx = int, reported_cases = matrix, disease_deaths = matrix, success = TRUE)`

When `precomputed_results = NULL` (default), both functions run LASER locally as before. Zero impact on non-Dask usage.

### Fallback

The entire post-cal Dask block is wrapped in `tryCatch`. If cluster creation or dispatch fails, a warning is logged and the existing local R parallel path runs unchanged:

```r
postca_dask <- tryCatch(
  .mosaic_postca_dask(...),
  error = function(e) {
    log_msg("WARNING: Post-cal Dask dispatch failed: %s", e$message)
    log_msg("  Falling back to local execution")
    NULL
  }
)
```

## Validation

Tested with ETH, 50 calibration sims, `best_model_n_sims = 5`, `ensemble_n_sims_per_param = 2`:

```
[07:26:43] Opening Dask cluster for post-calibration (10 workers)...
[07:30:08] Dispatching 5 ensemble sims...
[07:30:14] Gathered 5 ensemble results
[07:30:14] Dispatching 80 stochastic param sims (40 configs × 2 sims)...
...
[07:30:59] R2 ensemble: cases = 0.0768 (bias=1.22) | deaths = 0.0004 (bias=1.36)
```

- Post-cal cluster opened in ~3.5 min (Coiled cold start)
- 5 ensemble sims dispatched and gathered in 6 seconds
- 80 stochastic sims dispatched, gathered, and processed
- Ensemble R² correctly computed from precomputed results
- Full pipeline completed with exit code 0

## Alternatives considered

### Option A: Dispatch before cluster close

Move post-cal sim dispatch to before the calibration cluster closes. Rejected because:
- `config_best` and posterior param sets aren't available until after R-heavy post-processing
- Would require restructuring 500+ lines of post-calibration code
- Risk of TLS timeouts during the mixed R/Dask phase

### Option C: Keep calibration cluster alive

Don't close the calibration cluster; use it for post-cal sims too. Rejected because:
- The 500+ lines of R-heavy processing between cluster close and post-cal sims would starve the Dask event loop
- This is the exact pattern that caused the TLS heartbeat timeout bugs in the first place
- Paying for 100 workers while R processes sequentially is wasteful

## Cost impact

The post-cal cluster uses fewer workers (≤20 vs 100) and smaller VMs. For a typical run:
- Cluster startup: ~2-3 min (Coiled cold start) or ~30s (warm)
- Sim execution: 1-5 min depending on count
- **Total added cost**: ~$1-3 per run (vs hours of VM time for local execution)
