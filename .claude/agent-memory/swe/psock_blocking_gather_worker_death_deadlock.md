---
name: psock-blocking-gather-worker-death-deadlock
description: parLapply/pblapply(cl=) blocking unserialize() hangs the master FOREVER on Linux when a PSOCK worker PROCESS crashes; fix = socketSelect-timeout robust gather
metadata:
  type: project
---

A PSOCK worker that crashes its PROCESS (segfault / fatal C-level abort in the
embedded Python/numba/laser engine, or an OOM kill) is NOT the same as an R-level
error. The worker's `tryCatch` only catches R errors → `success=FALSE` records. A
process crash bypasses tryCatch entirely and leaves a half-closed socket.

`parallel::parLapply` / `pbapply::pblapply(cl=cl)` gather with a BLOCKING
`unserialize(node$con)`. On a dead peer socket this is platform-dependent:
- **macOS** surfaces it as `Error in unserialize(node$con): error reading from connection`.
- **Linux** can **BLOCK FOREVER** waiting on the dead socket.

The Linux block is the "0 CPU on master AND all workers, frozen" deadlock: one
worker crashed, the rest finished, the master is parked in unserialize() with no
timeout. Diagnostically it looks like a B2/priors-specific bug because changing the
sampled values changes which sim crashes — but the configs run fine standalone
(sequentially AND via a standalone calc_model_ensemble parallel call). The trigger
is environment-specific (high core count VM, libexpat-wrapped Rscript), the *hang*
is the blocking gather. Don't chase the config; fix the gather.

**Fix (MOSAIC v0.49.1):** `.mosaic_cluster_lapply_robust()` in
`R/calc_model_ensemble.R` replaces the blocking gather in the parallel branch.
Load-balanced manual dispatch:
- `socketSelect(cons, timeout = idle_timeout_sec)` waits on busy workers' sockets;
  a finite timeout means a silent/dead peer can never block the master forever.
- Read each ready node with `recvData(cl[[w]])` (NOT `recvOneResult(cl)`) so a
  failed read is attributed to the CORRECT worker (its in-flight task). A dead
  socket reads as an error → record `.mosaic_worker_died=TRUE, success=FALSE` for
  that task, retire the worker (`dead[w]<-TRUE`, never redispatch), degrade on
  survivors, and `warning()` the user.
- If no worker responds within `idle_timeout_sec` (default
  `getOption("MOSAIC.ensemble_worker_timeout_sec", 1800)`), `stop()` with a
  diagnostic naming the stalled task indices instead of hanging.
- `sendCall`/`recvData` via `utils::getFromNamespace(.,"parallel")` (R CMD check
  clean; `socketSelect` is base R). Node connection is `cl[[w]]$con`.

`recvData(node)` returns `list(type,value,success,time,tag)`. Index results by the
task tag I tracked (`node_task[w]`), NOT arrival order, to preserve input order.

Perf bonus: export BOTH `param_configs` AND `run_param_stoch_simulation` to worker
global env and dispatch a closure with `environment(.) <- .GlobalEnv`, so the
116-config payload is shipped ONCE (clusterExport), not re-serialized per task as
`pblapply(cl=)` did. Real B2 MOZ ensemble: 141s → 105s, 348/348 OK.

Tests: `tests/testthat/test-ensemble_cluster_robust.R` — happy path (order
preserved, tasks > workers), `tools::pskill(SIGKILL)` worker death degrades w/o
hang, sleeping worker → stop() within the idle timeout. PSOCK-only (skip guard).

Related: [[optimize_subset_levers]] (PSOCK-not-FORK + install-before-parallel-test).
