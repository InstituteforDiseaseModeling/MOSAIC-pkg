# Post-Calibration Ensemble Sims: Option A — Reconnect to Same Cluster

**Date**: 2026-04-04
**Branch**: `tt/azure-coiled`
**Status**: Implemented and validated

---

## Background

In v0.19.0, `run_MOSAIC_dask()` was merged into `run_MOSAIC()` via a `dask_spec` parameter. The Coiled cluster is intentionally closed before post-processing to prevent Python event loop starvation and TLS heartbeat timeouts during the R-heavy post-calibration phase (~500 lines of weight calculation, parquet I/O, convergence diagnostics, posterior estimation).

## Problem

Two post-calibration steps require LASER simulations:

| Step | Function | Default sims | Previous execution |
|------|----------|-------------|-------------------|
| Stochastic ensemble R² + plots | `calc_model_ensemble()` | `best_model_n_sims` (default 100) | Local R parallel |
| Parameter uncertainty ensemble | `plot_model_fit_stochastic_param()` | posterior params × `ensemble_n_sims_per_param` (up to ~10,000) | Local R parallel |

For a large Coiled run (50K sims on 100 workers, ~30 min), the parameter uncertainty ensemble alone could run for **hours** on the local machine.

## Solution: Disconnect Client, Keep Cluster, Reconnect

Instead of closing the entire Dask cluster, **disconnect only the client** before R-heavy post-processing. The Coiled cluster stays alive (via `idle_timeout`). After post-processing identifies `config_best` and posterior parameters, **reconnect to the same cluster** and dispatch post-cal sims.

### Flow

```
Calibration (Dask, 100 workers) → Gather results
→ Disconnect client (cluster stays alive, workers idle)
→ R-heavy post-processing (~60-90s, no TLS risk)
→ Reconnect to same cluster via dask_cluster$get_client()
→ Dispatch 30 ensemble sims → Gathered in 5s
→ Dispatch 200 stochastic sims → Gathered in 21s
→ Close cluster for real
→ calc_model_ensemble(precomputed_results=...) → 0s LASER, just aggregation
→ plot_model_fit_stochastic_param(precomputed_results=...) → 0s LASER, just plotting
→ Write summary
```

### Why this works

The TLS heartbeat issue is caused by the Dask client's Python event loop being starved while R blocks the main thread for extended periods. By disconnecting the client:
- No active TCP connection → no heartbeats needed → no timeouts
- The Coiled cluster stays running (billed but idle) during post-processing
- Workers keep their state (LASER module imported, Python warm)
- Reconnect via `dask_cluster$get_client()` inherits the TLS/SSL context from the cluster object

### Key implementation detail: TLS reconnect

Coiled uses TLS-encrypted scheduler connections. A bare `Client(address)` fails because it lacks the SSL context:

```
TypeError: TLS expects a `ssl_context` argument of type ssl.SSLContext
```

The fix: reconnect via `dask_cluster$get_client()` which inherits the TLS context from the Coiled cluster object (still alive in R's scope).

## Files Changed

| File | Change |
|------|--------|
| `R/run_MOSAIC.R` | Disconnect client (not cluster) at ~line 1214; reconnect via `dask_cluster$get_client()` at ~line 1808; dispatch post-cal sims; close cluster after gather |
| `R/run_MOSAIC_helpers.R` | Added `.mosaic_postca_dask()` — takes existing client + worker module, dispatches ensemble + stochastic sims |
| `R/calc_model_ensemble.R` | New `precomputed_results` param — skips local LASER when provided |
| `R/plot_model_fit_stochastic_param.R` | Same `precomputed_results` pattern |
| `inst/python/mosaic_dask_worker.py` | Added `run_laser_postca()` — lightweight worker for post-cal sims |

### `run_MOSAIC.R` — Disconnect/Reconnect

```r
# Before R-heavy post-processing (~line 1214):
client$close()   # disconnect only — dask_cluster stays alive
client <- NULL

# ... 500+ lines of R processing (combine results, weights, posteriors) ...

# After config_best is ready (~line 1808):
client <- dask_cluster$get_client()   # reconnect with TLS context
# dispatch sims, gather results
client$close()
dask_cluster$close()   # now shut down for real
```

### `.mosaic_postca_dask()` helper

Takes an existing `client` and `mosaic_worker` (no cluster creation). Dispatches:
1. **Ensemble sims**: one `config_best`, N different seeds → scatter matrices once, submit N tasks
2. **Stochastic sims**: M posterior configs × K seeds each → scatter matrices per config, submit M×K tasks

Returns precomputed results in the format expected by downstream functions.

### `run_laser_postca()` Python worker

Simpler than `run_laser_sim()` (calibration worker). Takes a complete config as JSON + scattered matrix fields, runs a single LASER iteration with a given seed. No parameter merging, no multi-iteration handling.

### `precomputed_results` interface

Both `calc_model_ensemble()` and `plot_model_fit_stochastic_param()` accept an optional `precomputed_results` parameter. When provided, they skip all local LASER execution and go straight to aggregation/plotting. When `NULL` (default), they run LASER locally as before — zero impact on non-Dask usage.

### Fallback

The entire reconnect + dispatch block is wrapped in `tryCatch`. If reconnection or dispatch fails, a warning is logged and the existing local R parallel path runs unchanged.

## Validation

Tested with ETH, 50 calibration sims, 30 ensemble sims, 200 stochastic sims (40 configs × 5):

```
[23:55:43] Disconnecting Dask client (cluster stays alive for post-cal reconnect)
[23:55:43]   Client disconnected

  ... 66 seconds of R-heavy post-processing (no TLS errors) ...

[23:56:49] Reconnecting to Dask cluster for post-cal sims...
[23:56:50]   Reconnected (5 workers)
[23:56:50] Dispatching 30 ensemble sims to Dask...
[23:56:55] Gathered 30 ensemble results               ← 5 seconds
[23:56:55] Dispatching 200 stochastic param sims (40 configs x 5 sims)...
[23:57:16] Gathered 200 stochastic results             ← 21 seconds
[23:57:16] Closing Dask cluster (post-cal sims complete)
[23:57:16] Computing ensemble R² (30 stochastic runs)...
[23:57:18] === Run Summary ===
[23:57:18]   R2 ensemble: cases = 0.0685, deaths = 0.0103
Done. Simulations: 50
```

- Disconnect/reconnect: no TLS errors
- Workers warm: 5 workers ready instantly after reconnect
- 230 LASER sims completed in **26 seconds** on Dask
- `calc_model_ensemble` + `plot_model_fit_stochastic_param` used precomputed results: **2 seconds** (aggregation only)
- Graceful fallback verified in earlier test (TLS reconnect failure → local execution completed successfully)

## Comparison with Option B (Second Cluster)

| | Option A (Reconnect) | Option B (Second Cluster) |
|---|---|---|
| Cluster startup | 0s (reuse existing) | 2-3 min (Coiled cold start) |
| Workers | Warm (LASER loaded) | Cold (need module import) |
| TLS handling | `dask_cluster$get_client()` | New cluster has own TLS |
| Idle cost during R processing | ~$0.10 (60-90s idle) | $0 (no cluster alive) |
| Total added latency | ~0s | ~3 min |
| Complexity | Lower (no cluster creation) | Higher (full cluster setup) |
| Failure mode | Falls back to local | Falls back to local |

Option A is preferred: faster, simpler, and workers are warm.
