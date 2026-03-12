# Dask Scale-Out: Troubleshooting Session (2026-03-12)

Issues investigated and resolved while scaling MOSAIC BFRS calibration to 20K-40K simulations across 200 Coiled workers.

---

## 1. Docker Hub Pull Rate Limits

**Problem:** Coiled workers hitting `"You have reached your unauthenticated pull rate limit"` when pulling `ttingidmod/mosaic-worker:latest` from Docker Hub.

**Solution:** Migrated to Azure Container Registry (ACR):
- Created `idmmosaicacr.azurecr.io` (Standard tier, westus2)
- Enabled anonymous pull (no auth needed, no rate limits)
- Created Coiled software env `mosaic-acr-workers` pointing to ACR image
- Updated `run_MOSAIC_dask.R` default from `mosaic-docker-workers` → `mosaic-acr-workers`

## 2. Docker Hub Auth Fallback

**Added but superseded by ACR:** Added `docker_login` and `environ` support to `dask_spec` in `run_MOSAIC_dask.R`, auto-forwarding `DOCKER_USERNAME`/`DOCKER_PASSWORD` env vars to Coiled workers.

## 3. TLS Disconnection During Post-Processing

**Problem:** `CommClosedError` — Dask client lost TLS connection to scheduler and failed to reconnect within 30 seconds, crashing the run.

**Root Cause:** The connection drops because the R process blocks the Python event loop during long R-side work. Since `reticulate` shares the main thread between R and Python, any long-running R computation prevents the Dask Tornado event loop from sending heartbeat messages. The scheduler eventually considers the client dead and drops the connection. The critical sections vulnerable to this are:

- **Likelihood loop** in `.mosaic_run_batch_dask()` — the `for (idx in ...)` loop computing likelihoods + writing parquet for each sim. With batch_size=20,000 this loop runs for ~20 minutes with no Python event loop activity.
- **Post-processing parquet load** in the main function — loading/combining all parquet files into a single data frame (~2 min for 20K files).
- **ESS + subset optimization + weight computation** — additional heavy R-side work after combining results.

**Solution:** Close the Dask client/cluster *before* post-processing starts. All worker results are already gathered at that point — Dask is no longer needed. This also stops billing for Coiled workers sooner. Added diagnostic logging (scheduler ping every 500 sims, timing at each stage, memory reporting) to detect and diagnose any future connection issues.

## 4. Scheduler VM Too Small

**Problem:** Scheduler was hardcoded to same VM type as workers (`Standard_D2s_v6`, 8 GB).

**Solution:** Added separate `dask_spec$scheduler_vm_types` field, defaulting to `Standard_D4s_v6` (16 GB).

## 5. OOM Kill During Parquet Loading

**Problem:** R process killed by kernel at ~8.3 GB RSS while loading 19,999 parquet files. Silent exit (no R error). Confirmed via `dmesg` showing OOM kill of the R process inside the Docker container.

**Solution (iterative):**
- First: added `rm()` + `gc()` after batch function returns — not enough
- Second: added `result_lookup[key] <- list(NULL)` and `params_list[idx] <- list(NULL)` *inside* the likelihood loop to free each sim's data immediately after processing
- Added `gc()` + memory logging before post-processing

## 6. `subscript out of bounds` Bug

**Problem:** `params_list[[idx]] <- NULL` removes the element and shrinks the list in R, shifting all subsequent indices.

**Fix:** Changed to `params_list[idx] <- list(NULL)` (sets to NULL in-place, preserving list length).

## 7. Diagnostic Logging Added

Added timing/progress logs at key points in `run_MOSAIC_dask.R`:
- Gather elapsed time
- Likelihood loop progress every 500 sims + scheduler ping
- Likelihood throughput (sims/s)
- Memory usage before post-processing
- Dask client health check

## 8. Orchestrator VM Sizing

**Analysis:** Local WSL2 is I/O bound (100% disk) + memory constrained (93%). Recommended `Standard_E4s_v6` (4 vCPU, 32 GB, memory-optimized) for running the orchestrator Docker container on Azure.

## 9. Housekeeping

- Created `azure/ACR_SETUP.md` documenting the full ACR setup
- Moved test script from `/tmp/mosaic_dask_test.R` to `azure/mosaic_dask_test.R`
- Updated docker run command to use ACR image + mounted script path
- Updated `azure/MOSAIC_DASK_GUIDE.md` and `azure/DOCKER_COILED_SUMMARY.md` with new image/env references

---

## Files Modified

| File | Changes |
|------|---------|
| `R/run_MOSAIC_dask.R` | ACR default, `scheduler_vm_types`, `docker_login`/`environ`, incremental memory freeing, diagnostic logging, early Dask close |
| `azure/ACR_SETUP.md` | New — full ACR setup documentation |
| `azure/mosaic_dask_test.R` | New — test script (moved from `/tmp`) |
| `azure/MOSAIC_DASK_GUIDE.md` | Updated image refs, docker run command, Coiled env name |
| `azure/DOCKER_COILED_SUMMARY.md` | Updated image refs, Coiled env name |
