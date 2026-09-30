---
name: rengine-cost-model
description: Measured per-sim/per-phase cost model for the pure-R engine (PR #122) - worker cost, gc share, shard size/read cost, ensemble RAM, PSOCK efficiency, and the load_all-vs-PSOCK installed-package trap
metadata:
  type: reference
---

Measured 2026-09-14 on a 10-core Apple-silicon laptop, R 4.5.3, PR-122 checkout via
`pkgload::load_all`, default config (J=40, T=1398). Use these as the arithmetic base for
any runtime/feasibility estimate; do not re-derive from the engine time alone.

**The load_all/PSOCK trap (most important).** `make_mosaic_cluster()` and
`calc_model_ensemble()` both do `library(MOSAIC)` inside the workers. Under
`pkgload::load_all()` the parent is the dev build but every PSOCK worker loads the
INSTALLED build from `.libPaths()`. Verified: parent 0.69.1 (has `run_simulation`),
workers 0.64.2 (does NOT). Any parallel timing/benchmark taken under `load_all` measures
the installed package. `R CMD INSTALL` first, or have the workers `load_all` too.

**Per-sim cost (steady state, single process, n_iterations = 1):**
- engine `run_simulation()` alone: 0.91 s (J=40) / 0.78 s (J=1)
- FULL calibration worker `.mosaic_run_simulation_worker()`: **1.43 s (J=40)**, 1.19 s (J=1)
  -- i.e. the engine is only ~64% of a sim. Rest: sample_parameters 0.13 s,
  calc_model_likelihood 0.04-0.11 s, parquet write 0.047 s, 2x gc 0.40 s.
- `gc.time()` says **0.71 s/sim = 50% of worker wall** is GC, but only ~0.40 s is the two
  explicit `gc()` calls; the remainder is automatic GC that removing them will not recover.
- At the DEFAULT `n_iterations = 3L` the two gc() calls are per worker TASK, so per-engine-run
  cost drops to 1.24 s. "sims/sec" is meaningless without recording n_iterations.

**Parallel efficiency (laptop, engine only, PSOCK):** k=5 -> 80%, k=10 -> **58%**; per-sim
inflates 1.71x at k=10 vs k=1. A P/E-core box (Apple silicon, 12700H) cannot produce an
interpretable efficiency curve -- same objection A-3a raised. `detectCores(logical=FALSE)`
returns 10 here (= logical count), so "physical cores" is not recoverable on Apple silicon.
Cluster spawn is ~5-7 s roughly independent of k (parallel setup strategy); `library(MOSAIC)`
alone is 3.2 s per worker.

**Shards / disk.** One `sim_%07d.parquet` per sim: **555 KB at J=40** (1352 cols, zstd-3),
26 KB at J=1. 40k sims = 22 GB, 100k = 55 GB transient (deleted after consolidation).
Reading them back is the hidden giant: `.mosaic_load_and_combine_results` uses a SERIAL
`lapply(chunk_files, arrow::read_parquet)` above 5000 files, measured **20-31 ms/shard warm**
=> ~16 min for 40k, ~41 min for 100k, single-threaded and unparallelizable.

**Ensemble phase RAM is NOT the "926 MB/worker" calibration figure.**
`calc_model_ensemble()` clusterExports the whole `param_configs` list -- one FULL sampled
config per subset member, measured **4.52 MB each at J=40** -- to EVERY worker. Best subset
is data-dependent in [30, 1000]. At 1000 members x 120 workers that is ~540 GB of worker RAM
(hedgehog's 448 GB OOMs). Master additionally allocates two dense
`array(40 x 1398 x n_param x n_stoch)` = 4.47 GB each at 1000x10. Trajectory capture spills
**3.26 MB per member** (gz RDS, 18 channels, J=40) to scratch -> 32.6 GB at 1000x10.

**Work volume is NOT pinned by the sim count.** Post-calibration runs
`n_best_subset x n_iter_ensemble(10) + n_iter_best(100)` further engine runs, and
`n_best_subset` comes from a grid search over the likelihood surface. For a 100-sim run the
post-calibration phase is 4-11x the calibration work. To pin total work you must also fix
`targets$min_best_subset == targets$max_best_subset`, `predictions$n_iter_ensemble`,
`n_iter_best`, and `capture_trajectories`.

**R connection ceiling: exactly 125** usable (128 total - 3 std streams), measured. PSOCK is
1 connection/worker, so `makeCluster(126)` fails at the 126th `socketConnection`; any
connection the caller holds open (log sink, CSV) lowers it. `make_mosaic_cluster()` validates
only `n_cores >= 1` -- no cap is enforced anywhere.

Related: [[psock-blocking-gather-worker-death-deadlock]], [[ensemble-artifact-slimming]],
[[2015-window-runtime-blast]], [[r-psock-128-ceiling]].
