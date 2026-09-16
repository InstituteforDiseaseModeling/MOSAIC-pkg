# MOSAIC calibration pipeline — performance improvements

Plan of record for speeding up the calibration pipeline **after** the engine migration
(PR #122) and the engine optimisation (PR #123) have landed. Written 2026-09-15.

Scope: everything in `run_MOSAIC()` that is **not** the transmission engine. PR #123
already took the engine 2.2–2.5x; this document is about the work that now dominates
around it.

Figures in sections 1-9 are measured on this laptop (Apple M1 Max, R 4.5.3) against the
PR-123 build at 40 locations unless stated, with the source named per row. **Section 1a
supersedes the laptop projections where the two disagree** — it is a 100,000-simulation,
40-location production run on dugong, added 2026-09-15, and it reordered the priorities
below. Nothing here is implemented.

---

## 1. Why this document exists

After PR #123 the engine is no longer the bottleneck everywhere, and where it is not,
nothing else has been optimised. Measured per-simulation cost, 40 locations,
`n_iterations = 3`:

| component | cost | share |
|---|---|---|
| 3 x `run_simulation()` | 1.011 s | **97%** |
| `sample_parameters()` (production call) | 0.035 s | 3% |
| likelihood + clamp + parquet write | < 0.05 s | ~1% |

At **one** location the picture inverts — the engine is ~20% and fixed orchestration
dominates, which is why the ETH 30k comparison showed only 1.39x end-to-end against a
2.2x engine win. So the improvements below matter most for small-J, many-simulation
work, and for the serial post-calibration stages that no amount of engine speed touches.

**A measurement error worth recording.** `sample_parameters()` was first clocked at
**16.2 s** at 40 locations, profiling to `print.default` at 99.8% of self time. That was
wrong: the benchmark used the default `verbose = TRUE`. The production call is
`verbose = FALSE, validate = FALSE` and costs **0.035 s** — a 460x difference. All four
hot call sites (`run_MOSAIC.R:212`, `run_MOSAIC.R:2433`, `calc_model_ensemble.R:454`,
`calc_Reff.R:529`) already pass `verbose = FALSE`; the remaining call sites are roxygen
examples. No defect — but the default is a 16-second trap for interactive use at 40
locations and could carry a note.

---

## 1a. Production measurement — 100,000 simulations, all 40 locations

Added 2026-09-15 after the plan was first written. Everything above this line was
projected from laptop microbenchmarks; this section is measured on **dugong**
(176 cores, 1.5 TiB), 100,000 simulations, 40 locations, `n_iterations = 3`,
80 workers per arm, FIXED mode, `plots = TRUE`, `capture_trajectories = TRUE`,
`optimize_subset = TRUE`. Both arms ran concurrently on the same box, so absolute
times carry some mutual contention; the phase *shares* are the durable result.

Arms: `main` = v0.66.0 (Python laser-cholera 0.16.1), `pr123` = the pure-R engine.
Logs: `dugong:~/prod_main.log`, `dugong:~/prod_pr123.log`. Resource sampler:
`dugong:~/res_100k_v4.csv`.

| phase | cores used | main (Python) | pr123 (R) | share of pr123 run |
|---|---|---:|---:|---:|
| simulation | 80 | 141.9 min | **97.5 min** | 37% |
| shard combine | **1** | 82.2 min | **95.8 min** | **37%** |
| weights + convergence | 1 | ~1 min | ~1 min | <1% |
| ensemble resim + predictions | 80 | 26.5 min | 30.2 min | 12% |
| `plot_model_distributions()` | **1** | — | 8.6 min | 3% |
| `plot_model_posteriors_detail()` | **1** | — | 20.0 min | **8%** |
| remaining figures + trajectories | **1** | — | 7.3 min | 3% |
| **total** | | did not finish in window | **260.2 min** | |

`run_MOSAIC()` itself reported `Calibration complete: 223.93 min`; the extra 36 min is
post-calibration figure rendering, which the plan had not costed at all.

**Both arms finished.** Final end-to-end, from `summary.json`:

| | main (Python) | pr123 (R) | ratio |
|---|---:|---:|---:|
| full pipeline incl. rendering | 302.6 min | 259.9 min | **1.16x** |
| calibration only (to `Calibration complete`) | 267.5 min | 223.9 min | **1.19x** |
| simulation phase only | 141.9 min | 97.5 min | **1.46x** |

The headline engine number at production scale is therefore **1.16-1.19x end-to-end**,
not the 2.2-2.5x the microbenchmarks show — because the serial phases that dominate the
run are identical on both arms. (The ratio is if anything flattered *towards* main: its
combine happened to run in 82.2 min against pr123's 95.8 min, purely from which arm held
the disk when. Normalising both to an equal combine puts the end-to-end ratio nearer
1.24x.) This is the strongest single argument for the rest of this document: the engine
work is done, and what is left is all in the serial tail.

**Cross-engine metric stability — evidence for item 4.** The two engines are independent
implementations with different RNG streams, so comparing the same metric across them
measures how stable that metric is. The tier ensemble agrees closely; the headline
(MAE-optimised) ensemble does not:

| metric | main | pr123 | spread |
|---|---:|---:|---:|
| `r2_cases_ensemble_tier` (n=114 / 117) | 0.6302 | 0.6287 | **0.2%** |
| `r2_deaths_ensemble_tier` | 0.4433 | 0.4373 | **1.4%** |
| `r2_cases_ensemble` (headline, n=90 / 52) | 0.5037 | 0.4776 | **5.5%** |
| `r2_deaths_ensemble` (headline) | 0.3574 | 0.3318 | **7.7%** |

The headline metric is roughly an order of magnitude more variable across engines than
the tier metric, because the MAE optimum picked a 90-member subset on one arm and a
52-member subset on the other from statistically equivalent pools. Both engines agree
on the science; they disagree on the number `summary.json` puts at the top.

**Three results that change this plan.**

1. **The engine is no longer the largest cost — the serial master is.** Summing the
   one-core rows: 95.8 + 8.6 + 20.0 + 7.3 = **131.7 min, 51% of the 260-minute run,
   executed on a single core while 79 sit idle.** The parallel phases total 127.7 min.
   PR #123 cut the simulation phase by 44 min; there is more than that available in
   work that is currently serial and embarrassingly parallel.

2. **Per-shard combine cost is 2.4-2.8x worse in production than the laptop projection**
   — 49.3 ms/shard (main) and 57.5 ms/shard (pr123) against the 20.2 ms/shard measured
   on the laptop at 1,000 columns. Production shards carry 1,352 columns and the two
   arms contended for the same disk. Item 1's saving is correspondingly understated;
   see the revision there.

3. **Peak memory is in the ensemble, not the simulation.** Sampled RSS-sum across the
   worker cohort:

   | phase | main | pr123 |
   |---|---:|---:|
   | simulation | 117.5 GB (1.46 GB/worker) | **74.1 GB (1.13 GB/worker)** |
   | combine (master alone) | 18.1 GB | 18.3 GB |
   | **ensemble resim** | **422.6 GB (19.3 GB/worker)** | **357.1 GB (23.6 GB/worker)** |

   System peak was 442 GB of 1.5 TiB. The ensemble stage costs **~17-20x per worker what
   the simulation stage costs**, at only 114 (main) / 52 (pr123) parameter sets. This
   run would not fit on hedgehog (448 GB). See the revision to item 5.

---

## 2. Improvement 1 — the shard combine takes the slow branch precisely when it matters

**Status: IMPLEMENTED (v0.79.0).** Measured on the laptop and in production.
**Saving: ~30-35 min per 100,000-simulation run** on the serial master — less than the
~35-42 min projected below, because correctness required `unify_schemas = TRUE`, which
costs about half the available speedup. See "What was implemented".

`.mosaic_load_and_combine_results()` (`R/run_MOSAIC_helpers.R:764`) branches on file
count:

```r
if (n_files <= chunk_size)   arrow::open_dataset(dir) |> collect()      # FAST
else                         rbindlist(lapply(files, read_parquet))      # SLOW
```

with `chunk_size = 5000L`. The fast path serves small runs; the slow path serves large
ones. Measured on 2,000 shards x 1,000 columns:

| strategy | ms/shard | projected at 100,000 shards |
|---|---:|---:|
| `lapply(read_parquet)` + `rbindlist` — **current, > 5000 files** | 20.22 | **34 min** |
| `open_dataset(dir)` |> `collect()` — current, <= 5000 files | 5.84 | 10 min |
| **`open_dataset(<file chunks>)`** — proposed | 11.47 | **19 min** |

At 80 columns (single-location) the same comparison is 3.63 vs 0.85 ms/shard, i.e.
`open_dataset` is 4.3x faster.

The in-code comment justifies the fallback on memory grounds, and that concern is real —
`open_dataset(dir) |> collect()` materialises everything at once and has OOMed at 40,000+
shards. But the remedy for memory is to **chunk the dataset**, not to drop to per-file
reads. `arrow::open_dataset()` accepts a file vector, so chunking preserves the memory
bound and keeps most of the speed.

Verified equivalent: `identical(dim(current), dim(proposed))` is `TRUE` and row counts
match (2,000 / 2,000 / 2,000).

Note the combine is also pinned to a single arrow thread, because
`.mosaic_set_all_thread_env()` sets `ARROW_NUM_THREADS=1` for worker safety. Whether the
master should raise it for the combine was a separate question; it has now been tested,
and the answer is **no**. Over 6,000 shards through the shipped `open_dataset` path:

| arrow cpu / io threads | time |
|---:|---:|
| 1 / 2 | 10.03 s |
| 2 / 2 | 9.20 s |
| 4 / 4 | 8.59 s |
| 8 / 8 | 8.45 s |

Eight times the threads buys **1.19x**. The combine is bound by per-file metadata parsing,
not by anything the thread pool parallelises, so raising `ARROW_NUM_THREADS` on the master
is not worth the worker-safety risk. Item 6b is the change that actually addresses this.

**Reconciliation with production (section 1a).** The `streaming` method does not use
`open_dataset` above `chunk_size`: it falls through to
`rbindlist(lapply(chunk_files, arrow::read_parquet))` per 5,000-file chunk
(`R/run_MOSAIC_helpers.R`, `streaming` branch, `n_files > chunk_size`). So at 100,000
shards it pays the per-file `read_parquet` cost 100,000 times; the chunking bounds
memory, not time. Measured cost per shard:

| | ms/shard | 100k shards |
|---|---:|---:|
| laptop projection, 1,000 cols | 20.22 | 34 min |
| **production, 1,352 cols (main)** | **49.3** | **82.2 min** |
| **production, 1,352 cols (pr123)** | **57.5** | **95.8 min** |

The laptop measured `open_dataset(<file chunks>)` at 11.47 ms/shard against
`lapply(read_parquet)`'s 20.22 — a **1.76x** improvement. Carrying that ratio to the
production per-shard cost projects **47-54 min**, i.e. a saving of **35-42 min**. This
is a projection, not a measurement: the ratio has not been re-measured at 1,352 columns
or on dugong's disk, and doing so is the first step of implementing this item.

### What was implemented

The `streaming` branch's `n_files > chunk_size` path now reads each chunk with
`arrow::open_dataset(chunk_files, format = "parquet", unify_schemas = TRUE)` instead of
`rbindlist(lapply(chunk_files, arrow::read_parquet), fill = TRUE)`. Chunking and
`chunk_size = 5000L` are unchanged, so the memory bound is unchanged.

**`unify_schemas = TRUE` is load-bearing, and the reason is a silent-corruption trap.**
Given a file vector, `open_dataset()` adopts the FIRST file's schema and does **not**
raise when a later file disagrees — it drops that file's extra columns and returns NA.
A `tryCatch` fallback around it is therefore dead code for the case it was written for
(the same shape as lesson #13's dead `is.null()` guard). The first implementation had
exactly that bug; `test-combine-shards.R` caught it. Measured at 1,352 columns,
1,000 single-row shards, min of 3:

| strategy | time | vs current | correct? |
|---|---:|---:|---|
| per-file `rbindlist(fill = TRUE)` — what this replaced | 21.47 s | 1.00x | yes |
| `open_dataset()`, default schema handling | 7.81 s | 2.75x | **no — drops columns silently** |
| **`open_dataset(unify_schemas = TRUE)`** — shipped | 13.68 s | **1.57x** | yes |

Correctness costs 1.18x here, and it is not negotiable: the union-schema read reproduces
`rbindlist(fill = TRUE)` exactly.

**Acceptance — met.** At 6,000 shards across a chunk boundary (2 chunks, `chunk_size`
5000): `identical(got, want)` is **TRUE** (exact, not merely `all.equal`), column names
and row order preserved, **1.74x** (18.13 s -> 10.40 s at 62 columns). Peak process RSS
is **unchanged** — 0.67-0.68 GB on both arms over three alternating pairs; R's own
`gc()` max-used is *lower* on the new path (Ncells 518 vs 699 MB) because it no longer
materialises one intermediate data.frame per file. Regression tests in
`tests/testthat/test-combine-shards.R` pin the fast path, the fill semantics, and the
small-file branch; the mismatched-schema test fails if `unify_schemas` is ever dropped.

**Projected production saving.** The laptop measures 21.47 s -> 13.68 s per 1,000 shards
at 1,352 columns, i.e. 35.8 min -> 22.8 min projected at 100k. Scaling by the 2.4-2.8x
laptop-to-dugong factor established in section 1a, the production combine should fall
from the measured 82-96 min to roughly **55-64 min** — a saving of **~30-35 min**. This
has not yet been confirmed on dugong at 100k; that is the next measurement.

---

## 3. Improvement 2 — `calc_Reff()` is fully serial

**Status:** confirmed by inspection, not implemented.
**Saving:** ~5–6 min per R_eff computation at 40 locations.

`grep -cE "mclapply|parLapply|makeCluster|foreach|n_cores" R/calc_Reff.R` returns **0**.
`.mosaic_reff_resim_ci()` (`R/calc_Reff.R:566-598`) re-simulates every ensemble member in
a nested `for (p) for (s)` loop, and explicitly pins BLAS to one thread at `:506`.

At `run_MOSAIC()` defaults (`n_iter_ensemble = 10L`) with ~100 parameter sets that is
~1,000 sequential engine runs. At the measured 0.34 s/run (40 locations, PR-123 engine)
that is ~6 minutes of single-threaded work that would be ~30 s on 20 cores.

It is not called from `run_MOSAIC()`, so it costs nothing during calibration — it is the
largest single-threaded block on the post-hoc R_eff path. It already takes a cluster-free
design, so adding an optional `cl =` argument mirrors what `optimize_ensemble_subset()`
already does.

**Acceptance:** bit-identical `reff_loc` output against the serial path (the per-member
seed is deterministic, so this should hold exactly); the existing faithfulness gate
unchanged.

---

## 4. Improvement 3 — per-worker shard directories

**Status:** hypothesis, unmeasured. Test before implementing.
**Possible saving:** unknown; the candidate explanation for a measured 16–33% parallel
efficiency loss.

Measured parallel efficiency through the real calibration worker (engine + likelihood +
parquet into one shared directory): **84% at k=8 on PR-123, 67% on PR-122, 75% on main.**
At 80 workers on dugong the same path sustains roughly 70 file creations/s of
tmp-then-rename traffic into a single directory.

The engine itself is embarrassingly parallel and does no I/O, so the shared write target
is the most plausible non-scaling component. Writing to `samples/w<NN>/` per worker and
globbing across subdirectories at combine time would remove the contention if that is
what it is.

**Do not implement before measuring.** The test is cheap: run the existing
`parallel/throughput` benchmark workload with per-worker subdirectories and compare
`sims_per_sec` at k = 1/4/8. If the curve does not move, the loss is memory bandwidth or
core topology and this change buys nothing.

---

## 5. Improvement 4 — `optimize_subset`: a flat objective, and a headline metric that disagrees with it

**Status: RESOLVED (v0.81.2) — but not for the reason recorded below.** Two real
defects were found and fixed; the flat-objective account in this section turned out
to be wrong. See `claude/perf_100k_main_vs_pr123/eth_ab_mask_fix/README.md`.

**What was actually wrong.** `optimize_ensemble_subset()` builds
`ensemble_optimized` without copying `artifact_mask`. `run_MOSAIC()` then scores every
HEADLINE metric off that object, masking by `ensemble$artifact_mask` -- now NULL, so
`.mosaic_mask_central_for_scoring()` silently substituted its `cases_warmup = 2`
fallback for the run's real scored window (`score_idx_cases = 31` on the 100k runs).
The headline R2 was therefore computed over 28 timesteps the calibration likelihood
excludes, in every run with `optimize_subset = TRUE`. A controlled Ethiopia A/B at
5,000 simulations -- identical draws, likelihoods, subsets and ESS -- moved
`r2_cases_ensemble` from 0.4395 to 0.7987 against a tier value of 0.7978, closing a
44.9% gap to 0.1%; deaths moved 0.0970 to 0.3544.

Separately, the optimiser never applied the mask it was handed, so SELECTION scored
excluded cells. That is fixed too, and it is what made the first defect visible -- but
measured over both 100k ensembles it **changed no selection at all** (argmax 90 -> 90
and 52 -> 52) and did not sharpen the objective (span 4.09% -> 4.35% / 4.68%).

**So the hypothesis below is disproved.** The flat objective and the noise-picked
argmax are real and remain unexplained; they were not caused by the masking. The
cross-engine headline spread, however, was: it is gone once the headline is scored on
the same window as the tier metric.

The original (incorrect) diagnosis is kept below because it is what the 100k data
looked like before the cause was found.

In the ETH 30,000-simulation run, **both** engines returned `n_best_subset = 30` — the
`min_best_subset` floor — out of ~27,000 retained draws. The grid search ran to
completion and selected the smallest permitted subset on both arms.

**The floor collapse did not reproduce at 100k / 40 locations.** pr123 selected
`n_best_subset = 52` from a 117-member tier subset, well clear of the floor of 30. The
full score profile (`dugong:~/prod-pr123/output_100k/3_results/figures/diagnostics/optimization_diagnostics.csv`,
34 candidate sizes from n=30 to n=117) confirms the search is correct under its own
objective: `score` is maximised at exactly n=52 (-2.0645), the global maximum of the
profile. No defect in the optimiser.

Two real problems remain, and they are different from the one originally recorded:

**(a) The objective is nearly flat.** Across the whole candidate range the score spans
-2.0645 to -2.1506 — **4%** — and `mae_cases` spans 16.75 to 17.45, also 4%. The search
evaluates 34 candidate subset sizes, each a weighted-median gather over the 4D
prediction arrays, to discriminate between options that differ by 4% on the quantity it
is optimising. `optimize_ensemble_subset()` already returns a `stability_flag` for
"score profile was flat"; nothing consumes it to short-circuit the search.

**(b) The headline R-squared is reported from the MAE optimum.** Section 1a adds the
decisive evidence: across two independent engine implementations the *tier* metric agrees
to 0.2% (cases) and 1.4% (deaths), while the *headline* metric disagrees by 5.5% and 7.7%
— an order of magnitude more variable, because the MAE optimum landed on 90 members on
one arm and 52 on the other from statistically equivalent pools. The default objective
is `"mae"` (normalised MAE), not R-squared, so the two need not agree — and at 100k they
do not. `summary.json` reports:

| | n | r2_cases | r2_deaths |
|---|---:|---:|---:|
| `*_tier` (Akaike subset) | 117 | **0.6287** | **0.4373** |
| headline (MAE-optimised) | 52 | **0.4776** | **0.3318** |

So the number a reader takes as the fit of the run is **24% worse** on both channels than
the tier ensemble computed from the same simulations. That is a reporting question, not a
performance one, but it is decided by this switch and should be settled before
`optimize_subset = TRUE` becomes a default.

The consequence is visible in the metrics: `r2_cases_ensemble` was 0.1764 (main) vs
0.2994 (pr123), a 70% spread, while the `_tier` variants computed over 113/114 draws
agreed to **0.2%** (0.7841 vs 0.7855). A posterior ensemble built from 30 of 27,000 draws
is dominated by a handful of weights and is unstable; the tier metric is not.

So an expensive grid search is (a) returning a degenerate answer and (b) producing the
headline ensemble metric from it. Either the objective genuinely prefers 30 — in which
case the search could short-circuit — or the search is broken at large N. Either way it
should be understood before it is optimised, and `optimize_subset = TRUE` should probably
not be a default until it is.

**Acceptance:** (a) a short-circuit driven by the existing `stability_flag` when the
score profile is flat within a stated tolerance, with the selected n and the flat range
both recorded; (b) a decision, with the maintainer, on which ensemble `summary.json`
should report as headline. Neither is a speedup; (b) can change published numbers and
must not be bundled with anything else.

---

## 6. Improvement 5 — guard the ensemble config broadcast

**Status:** **measured in production — the ensemble is now the memory peak of the whole
pipeline.** Prevents an OOM; not a speedup.

`calc_model_ensemble()` `clusterExport`s the full list of sampled configs to every
worker (`R/calc_model_ensemble.R:635`). A sampled config measures **4.52 MB** at 40
locations. At `n_subset = 1000` x 80 workers that is ~360 GB of broadcast, before any
simulation runs. The master separately allocates
`array(NA_real_, c(nL, nT, n_param_sets, n_iter))` **twice** — 4.47 GB each at
`nL=40, nT=1398, n_param=1000, n_iter=10`.

This did not bite in the runs to date only because improvement 4 collapsed the subset to
30. If the subset were large it would OOM on hedgehog (448 GB) before producing anything.

**Production measurement (section 1a) makes this concrete.** At only 52 (pr123) / 114
(main) parameter sets, the ensemble stage already peaks at **357 GB / 423 GB** of
worker RSS — 23.6 GB and 19.3 GB per worker respectively, against 1.1-1.5 GB per worker
during simulation. System peak was 442 GB. Two consequences:

* **This run already would not fit on hedgehog (448 GB).** The guard is no longer
  hypothetical protection against a subset size nobody has used; it is the difference
  between this exact workload running and not running on the smaller VM.
* The per-worker cost scales with the subset, so the plan's `n_subset = 1000` scenario
  is not 20x this figure by broadcast alone — the master's two
  `array(NA_real_, c(nL, nT, n_param_sets, n_iter))` allocations (4.47 GB each at
  n_param=1000) are on top of it.

The guard should report the computed broadcast size and the two array sizes, so the
message names the number rather than guessing.

**Fix:** compute the broadcast size up front and either refuse with a clear message or
chunk the dispatch. A guard that states the number is worth more than one that guesses.

---

## 6a. Improvement 6 — post-calibration figure rendering is 36 min on one core

**Status: IMPLEMENTED (v0.80.0; two defects fixed in v0.81.0).**
**Saving:** up to ~30 min per 40-location run with `plots = TRUE`. The measured
speedup is still outstanding — see section 10.

Two things went wrong in v0.80.0 and both were invisible to the tests shipped with
it, for the same reason: a cluster that fails to start falls back to `lapply()` and
produces byte-identical figures, so asserting output equality proves nothing about
whether any worker ran. (i) `make_mosaic_cluster()` stops unless
`set_root_directory()` has been called, which rendering never does — it now takes
`require_root = FALSE`. (ii) R serialises a function that is a NAMESPACE BINDING by
reference, so `.mosaic_traj_render_worker` came back "object not found" on any worker
whose installed build predated it; it is reparented to `globalenv()` before dispatch,
which is what `.mosaic_run_batch()` already does to its `worker_func`. The tests now
assert the cluster is really built and that files actually come out of it.

`run_MOSAIC()` calls `render_MOSAIC_figures()` (`R/run_MOSAIC.R:2761`) after
`Calibration complete`, and the whole of it is serial. Bracketed by output-file mtimes
on the pr123 100k run:

| step | span | output | cost |
|---|---|---|---:|
| `plot_model_distributions()` | 20:47:16-20:55:52 | 40 `distributions_<ISO>_Prior_Posterior.pdf` | 8.6 min |
| `plot_model_posteriors_detail()` | 20:55:58-21:15:59 | **286 PDFs** (40 locations x 7 groups + 6 global) | **20.0 min** |
| sensitivity, correlation, ensemble, ppc, spatial, `plot_model_trajectories()` | 21:15:59-21:23:16 | incl. 40 x (PDF + PNG) trajectories | 7.3 min |

That is **36 minutes, 14% of the run**, on one core, producing ~370 independent files.
Every one of these loops is per-location or per-(location x group) with no shared state
between iterations — the same shape as item 2, and the largest single-threaded block
left after item 1.

Two caveats before implementing:

* **Graphics devices are not automatically fork-safe or thread-safe.** The parallel unit
  must be a whole `open device -> draw -> close device` per file inside a PSOCK worker,
  never a shared device. PSOCK (not FORK) also avoids inheriting a device handle.
* **The cluster may still be alive at this point.** Check whether `run_MOSAIC()` has torn
  the calibration cluster down before `render_MOSAIC_figures()` is reached; reusing it is
  free, and creating a second one costs the usual PSOCK spin-up.

**Do this after item 1** — item 1 is larger (95.8 min vs 36 min), lower risk, and does
not touch rendering.

**Acceptance:** identical file set and identical file count against the serial path;
spot-check that a sample of PDFs is byte-identical or visually identical (fonts and
device metadata may differ across workers, so byte-identity is a bonus, not a
requirement); recorded render wall time before and after.

---

## 6b. Improvement 7 — one parquet per simulation is the wrong shard granularity

**Status: MACHINERY IMPLEMENTED (v0.80.1, v0.81.0), DEFAULT STILL OFF.**
**Possible saving: 60-90 min per 100k run, plus the same again on any resume.**

`control$io$shard_batch_size` (default `1L`) makes one parquet carry N simulations.
At the default the behaviour is byte-for-byte unchanged: a one-id chunk keeps the
historical `sim_%07d.parquet` name and the sequential path still runs the original
per-simulation worker. `.mosaic_resume_scan()` takes ids from the `sim` COLUMN rather
than the filename (v0.80.1), so a 100-row shard scans identically to 100 one-row
shards. **Not yet validated end to end at a real batch size, and the default has not
been changed** — see section 10.

Found while implementing item 1. The combine is slow because of **file framing, not
data volume**, and item 1 only makes the framing cheaper to parse rather than removing
it.

A single-row parquet with 1,352 columns is **444 KB on disk** (measured) while the row
itself is ~8 KB. That is ~330 bytes of parquet column metadata per column per file, and
it is why the 100k run wrote **52.9 GB of shards for 1.03 GB of data** — a 51:1 ratio,
straight from the production log (`Loading 100000 simulation files (52924.1 MB on
disk)` ... `Results in memory: 1031.7 MB`).

Measured at 1,352 columns, 1,000 simulations, min of 3, varying only how many rows go
in each file:

| rows/file | files | disk MB | per-file read | `open_dataset` |
|---:|---:|---:|---:|---:|
| **1 (current)** | 1000 | **434.1** | 20.62 s | 8.02 s |
| 10 | 100 | 54.0 | 1.82 s | 0.80 s |
| 100 | 10 | 15.2 | 0.17 s | 0.10 s |
| 500 | 2 | 11.5 | 0.04 s | 0.04 s |

Batching 100 simulations per file is **121x faster to read and 29x smaller on disk**
than the current layout — against item 1's 1.57x. Extrapolated to the production run,
the 95.8-minute combine becomes roughly **1-2 minutes**.

**It also fixes a second cost that item 1 does not touch.** `.mosaic_resume_scan()`
validates shards by *reading every one of them* (`R/run_MOSAIC_helpers.R`), on the
stated reasoning that "Shards are one row, so the read is cheap". At 1,352 columns and
444 KB per file it is not cheap — a resume of a 100k run pays the full combine cost a
second time before it does any work.

**Why this needs a decision rather than an implementation.** The shard filename is load
bearing:

* `.mosaic_parse_sim_ids()` derives the sim id from `^sim_0*([0-9]+)\.parquet$`.
* `.mosaic_resume_scan()` derives both the **count** (`n`) and the **watermark**
  (`max(sim_id)`) from those ids, and `.mosaic_reconstruct_state()` treats the watermark
  as authoritative for the next sim id so that a resume never reuses a seed.
* A crash mid-batch currently loses at most one simulation; with 100 rows per file it
  would lose up to 100 unless workers flush partial batches.

So batching means a naming scheme that encodes a range (or a scan that reads the `sim`
column rather than the filename), and a decision about the crash-granularity trade.
Neither is hard, but both change behaviour that resume correctness depends on, and that
is the maintainer's call — not something to bundle into a performance patch.

**Recommended:** take this as its own piece of work after items 6a and 5, with a
proposal covering the naming scheme, the resume-scan change, and the partial-flush
policy. It is the largest single saving identified anywhere in this document.

---

## 7. Not problems (checked, recorded so they are not re-investigated)

* **No O(N^2) rescan in FIXED mode.** The adaptive loop recomputes ESS and weights per
  batch, but fixed mode dispatches all N in a single batch, so that cost is paid once.
  It would matter in adaptive mode at large budgets.
* **`n_iterations` amortisation is already correct** — one `sample_parameters()` and one
  parquet write per task against `n_iterations` engine runs.
* **Thread pinning inside the calibration path is correct** — all six thread variables
  plus `ARROW_NUM_THREADS` are `"1"` in a live worker, set in the parent before spawn so
  PSOCK workers inherit them before `arrow` binds its pool.
* **The per-simulation `gc()` calls are already gone** (PR #123), measured at 292 ms and
  14.8% of the worker budget across the two call sites.
* **Worker memory is already much better** — median worker RSS at 40 locations is
  **922 MB (PR-123)** against **1,485 MB (main/Python)**, a 1.61x reduction measured live
  on dugong at 80 workers per arm.

---

## 8. Sequencing

Revised 2026-09-15 after the production measurement. Ordering now follows measured
wall-clock, not laptop projections.

| # | change | saving (100k, 40 loc) | effort | risk |
|---|---|---|---|---|
| 1 | chunked `open_dataset` in the combine | **~35-42 min** (projected from a measured 1.76x) | ~1 h | low, output verifiable |
| 6a | parallelise `render_MOSAIC_figures()` | **~30 min** (measured 36 min serial) | ~1 d | low-medium, device safety |
| 5 | guard the ensemble broadcast | prevents OOM; 357-423 GB measured | ~2 h | low |
| 2 | parallelise `.mosaic_reff_resim_ci()` | ~5 min / R_eff call (post-hoc path only) | ~0.5 d | low, bit-identical expected |
| 3 | measure per-worker shard dirs | unknown — measure first | ~2 h | none (measurement) |
| 4 | `optimize_subset` flat profile + headline metric | correctness | ~0.5 d | medium, may change published numbers |
| 6b | batch N simulations per shard file | **60-90 min / 100k, again on resume** | ~2 d + design | **high — changes resume semantics** |

Do **1** first: it is the single largest cost in the pipeline (37% of the run), it is
contained, and its acceptance test is exact. Then **6a**, the second largest, which is
independent of it. Then **5**, which is cheap and is the difference between this
workload running and not running on hedgehog.

**Item 6b is the largest saving in this document and is deliberately last in the table,
not last in value.** It needs a design decision on shard naming, the resume scan, and
crash granularity before any code is written; see section 6b.

Items 2 and 3 are smaller than they looked before the production run — 2 is on the
post-hoc R_eff path that `run_MOSAIC()` does not call, and 3 is a measurement whose
answer may be "nothing to fix". Item 4 is the only one that can change published model
output and must not be bundled with the others.

**Each item gets its own commit and version bump**, per CLAUDE.md.

---

## 9. How to measure any of this

The benchmark suite added alongside this work lives in `inst/bench/` (11 workloads,
frozen fixtures, interleaved multi-arm driver, per-arm resource sampling). Use it rather
than ad-hoc timing:

```sh
Rscript inst/bench/run_bench.R --lib=<library> --arm=<name> --src=<repo> --out=bench.csv
sh inst/bench/run_matrix.sh out.csv 4          # interleaved, multi-version
```

Two rules it enforces that matter here: **`min` is the headline statistic**, not the
mean (timing contamination is one-sided, and min-of-5 carries 2.98% relative sd against
median-of-5's 6.22%); and **arms are interleaved, never blocked** (an identical binary
drifted 7.8% over 20 minutes on a quiet machine).

Effects below roughly **7%** are not resolvable by wall clock on this hardware and should
be measured at the call site with a 10^5-iteration microbenchmark instead.

---

## 10. Outstanding (as of v0.81.2)

Status of every item, so this is the only place anyone has to look.

### Done

| # | change | shipped | caveat |
|---|---|---|---|
| 1 | chunked `open_dataset` in the combine | v0.79.0 | **production saving never confirmed** — the ~30-35 min is a projection carrying a laptop-measured 1.57x ratio across to dugong. One 100k run verifies it. |
| 6a | parallelise `render_MOSAIC_figures()` | v0.80.0, fixed v0.81.0 | **speedup number outstanding** — the first benchmark was invalidated by the two defects; the re-run is what produces the figure. |
| 4 | `optimize_subset` scoring mask | v0.81.2 | fixed, but not for the reason this document originally gave. See the open question below. |
| 6b | shard-batching machinery | v0.80.1, v0.81.0 | **default still `1L`**; needs an end-to-end run at a real batch size before flipping it. |

### Not started

| # | change | why it still matters | effort |
|---|---|---|---|
| 5 | guard the ensemble config broadcast | The ensemble stage peaks at **357-423 GB** at only 52-114 parameter sets (section 1a). The 100k workload already would not fit on hedgehog's 448 GB. Cheapest item left, and the only one that turns a crash into a clear message. | ~2 h |
| 2 | parallelise `.mosaic_reff_resim_ci()` | ~5 min per R_eff call, but on the post-hoc path `run_MOSAIC()` never calls — so it buys nothing during calibration. | ~0.5 d |
| 3 | measure per-worker shard directories | A measurement, not a change. Its answer may be "nothing to fix", and item 6b may make it moot by cutting file creations ~100x. | ~2 h |

### Open question, downgraded

Item 4's **flat objective is still unexplained**: the score spans ~4% over
n = 30..117, and the argmax landed on 90 (main) and 52 (pr123) from statistically
equivalent pools. Applying the scoring mask did not change that.

It is no longer a *correctness* concern. With `artifact_mask` propagated, the masked
R2 at those two subset sizes is 0.6426 (n=52) against 0.6406 (n=117) on pr123 — so
the flat profile means many subset sizes really are equally good, and the headline
metric no longer swings with the choice. What remains is a performance nit: the search
evaluates ~34 candidates to separate options differing by 4% on its own objective, and
`optimize_ensemble_subset()` already returns a `stability_flag` for "score profile was
flat" that nothing consumes.

### Recommended next

**Item 5**, then confirm **1** and **6a** with one production run each, then validate
**6b** and flip its default. Items 2 and 3 are the smallest and can wait.
