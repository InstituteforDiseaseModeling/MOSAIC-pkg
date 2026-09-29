---
name: psock-export-and-dead-guard-traps
description: Four R-level traps found auditing run_MOSAIC (v0.84.0 review) — clusterCall closures shadow clusterExport, isTRUE() on a control list is always FALSE, arrow open_dataset branch asymmetry in the shard combine, and x[[i]]<-NULL shrinking a preallocated gather list
metadata:
  type: reference
---

Durable R/PSOCK traps, each verified by experiment during the v0.84.0 deep review of
`R/run_MOSAIC.R`. Scripts live in `MOSAIC-pkg/claude/review_v084/scratch/PIPE-A/`.

## 1. `clusterCall(cl, function() {...})` ships the CALLER'S FRAME and shadows `clusterExport`

A closure created inside a function has that function's frame as its environment, so
`parallel::clusterCall(cl, function() { assign(".w", function() config, envir = .GlobalEnv) })`
serialises the whole calling frame to every worker — and the installed `.w` then resolves
`config` from **that** copy, not from the `clusterExport`ed globals. The export becomes dead
code and the payload travels twice; both copies stay resident per worker.

Proof (`exp_closure_env.R`): export a sentinel as `"FROM_EXPORT"` while the frame holds
`"FROM_FRAME"` —
```
as-written                      -> worker returns FROM_FRAME
environment(g) <- globalenv()   -> worker returns FROM_EXPORT
```
Measured payload in `run_MOSAIC`: 0.41 MB (1 location) / **10.12 MB (40 locations x 3,322
steps)**, duplicated at 125 workers ≈ 1.27 GB of avoidable transfer + RSS.

Fix: `environment(f) <- globalenv()` before dispatch, keeping the export.
`.mosaic_run_batch()` already does this for its `worker_func`; the `clusterCall` site in
`run_MOSAIC` did not (as of v0.85.0). Check any new `clusterCall`/`clusterEvalQ` for it.

## 2. `isTRUE(control$<section>)` is FALSE forever

Every `control` section (`io`, `paths`, `parallel`, `likelihood`, ...) is a LIST, so
`isTRUE()` on it can never be true. `run_MOSAIC.R:992` guarded the `mobility_tau_ci.csv`
artifact with `if (isTRUE(control$io))` and the block had been dead since v0.53.0 — zero such
files existed anywhere on disk. Same shape as CLAUDE.md lesson #13: a guard that is false by
construction, invisible because the block's only output is an optional file. When a comment
says "gated by io, not by plots", the author meant *no gate*; grep the artifact name across
the repo and `find` for it on disk to confirm a guard ever fires.

## 3. `.mosaic_load_and_combine_results()` has two inequivalent branches

`n_files <= chunk_size` (default 5000) takes `arrow::open_dataset(dir_params)` — the
**directory**, with the **default schema**. The `> chunk_size` branch takes the
`^sim_.*\.parquet$` file vector with `unify_schemas = TRUE`, and carries a 20-line comment
saying the default is "not merely faster, it is silently incorrect" (open_dataset adopts the
first fragment's schema and NAs a later shard's extra columns, with no error).

Verified: two shards where only the second has `cfr_ETH` -> small branch returns 6 columns
(**column lost**), chunked branch returns 7. Amplifier: `shard_batch_size` (v0.81.0) moved
production onto the unsafe branch — 100k sims at 100 rows/shard = 1,000 files <= 5,000.
Negative result worth keeping: `.quarantine/` is safely invisible to the directory scan
because arrow skips dot-prefixed paths (a subdir without the dot IS picked up).

## 4. `results[[i]] <- NULL` deletes the slot

In a preallocated gather (`results <- vector("list", n)`), assigning a task's `NULL` return
with `[[<-` removes the element and shortens the list, shifting every later index — silent
misalignment between `results` and `X`. `.mosaic_cluster_lapply_robust()`
(`R/calc_model_ensemble.R:273`) has this shape; it is currently unreachable only because no
worker there returns `NULL`. Use `results[i] <- list(value)`.

Related: [[psock-blocking-gather-worker-death-deadlock]], [[project-optimize-subset-levers]],
[[reference-rengine-cost-model]].
