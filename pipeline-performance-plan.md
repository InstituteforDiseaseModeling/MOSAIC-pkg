# MOSAIC calibration pipeline — performance improvements

Plan of record for speeding up the calibration pipeline **after** the
engine migration (PR \#122) and the engine optimisation (PR \#123) have
landed. Written 2026-09-15.

Scope: everything in
[`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
that is **not** the transmission engine. PR \#123 already took the
engine 2.2–2.5x; this document is about the work that now dominates
around it.

All figures are measured on this laptop (Apple M1 Max, R 4.5.3) against
the PR-123 build at 40 locations unless stated, with the source named
per row. Nothing here is implemented.

------------------------------------------------------------------------

## 1. Why this document exists

After PR \#123 the engine is no longer the bottleneck everywhere, and
where it is not, nothing else has been optimised. Measured
per-simulation cost, 40 locations, `n_iterations = 3`:

| component | cost | share |
|----|----|----|
| 3 x `run_simulation()` | 1.011 s | **97%** |
| [`sample_parameters()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/sample_parameters.md) (production call) | 0.035 s | 3% |
| likelihood + clamp + parquet write | \< 0.05 s | ~1% |

At **one** location the picture inverts — the engine is ~20% and fixed
orchestration dominates, which is why the ETH 30k comparison showed only
1.39x end-to-end against a 2.2x engine win. So the improvements below
matter most for small-J, many-simulation work, and for the serial
post-calibration stages that no amount of engine speed touches.

**A measurement error worth recording.**
[`sample_parameters()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/sample_parameters.md)
was first clocked at **16.2 s** at 40 locations, profiling to
`print.default` at 99.8% of self time. That was wrong: the benchmark
used the default `verbose = TRUE`. The production call is
`verbose = FALSE, validate = FALSE` and costs **0.035 s** — a 460x
difference. All four hot call sites (`run_MOSAIC.R:212`,
`run_MOSAIC.R:2433`, `calc_model_ensemble.R:454`, `calc_Reff.R:529`)
already pass `verbose = FALSE`; the remaining call sites are roxygen
examples. No defect — but the default is a 16-second trap for
interactive use at 40 locations and could carry a note.

------------------------------------------------------------------------

## 2. Improvement 1 — the shard combine takes the slow branch precisely when it matters

**Status:** measured, fix specified, not implemented. **Saving:** ~15
min per 100,000-simulation run, entirely on the serial master.

`.mosaic_load_and_combine_results()` (`R/run_MOSAIC_helpers.R:764`)
branches on file count:

``` r
if (n_files <= chunk_size)   arrow::open_dataset(dir) |> collect()      # FAST
else                         rbindlist(lapply(files, read_parquet))      # SLOW
```

with `chunk_size = 5000L`. The fast path serves small runs; the slow
path serves large ones. Measured on 2,000 shards x 1,000 columns:

| strategy | ms/shard | projected at 100,000 shards |
|----|---:|---:|
| `lapply(read_parquet)` + `rbindlist` — **current, \> 5000 files** | 20.22 | **34 min** |
| `open_dataset(dir)` | \> `collect()` — current, \<= 5000 files | 5.84 |
| **`open_dataset(<file chunks>)`** — proposed | 11.47 | **19 min** |

At 80 columns (single-location) the same comparison is 3.63 vs 0.85
ms/shard, i.e. `open_dataset` is 4.3x faster.

The in-code comment justifies the fallback on memory grounds, and that
concern is real — `open_dataset(dir) |> collect()` materialises
everything at once and has OOMed at 40,000+ shards. But the remedy for
memory is to **chunk the dataset**, not to drop to per-file reads.
[`arrow::open_dataset()`](https://arrow.apache.org/docs/r/reference/open_dataset.html)
accepts a file vector, so chunking preserves the memory bound and keeps
most of the speed.

Verified equivalent: `identical(dim(current), dim(proposed))` is `TRUE`
and row counts match (2,000 / 2,000 / 2,000).

Note the combine is also pinned to a single arrow thread, because
`.mosaic_set_all_thread_env()` sets `ARROW_NUM_THREADS=1` for worker
safety. Whether the master should raise it for the combine is a separate
question worth testing.

**Acceptance:** byte-identical `samples.parquet` against the current
path on a run of \> 5,000 shards; peak master RSS no higher than the
current chunked path; recorded combine wall time before and after.

------------------------------------------------------------------------

## 3. Improvement 2 — `calc_Reff()` is fully serial

**Status:** confirmed by inspection, not implemented. **Saving:** ~5–6
min per R_eff computation at 40 locations.

`grep -cE "mclapply|parLapply|makeCluster|foreach|n_cores" R/calc_Reff.R`
returns **0**. `.mosaic_reff_resim_ci()` (`R/calc_Reff.R:566-598`)
re-simulates every ensemble member in a nested `for (p) for (s)` loop,
and explicitly pins BLAS to one thread at `:506`.

At
[`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
defaults (`n_iter_ensemble = 10L`) with ~100 parameter sets that is
~1,000 sequential engine runs. At the measured 0.34 s/run (40 locations,
PR-123 engine) that is ~6 minutes of single-threaded work that would be
~30 s on 20 cores.

It is not called from
[`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md),
so it costs nothing during calibration — it is the largest
single-threaded block on the post-hoc R_eff path. It already takes a
cluster-free design, so adding an optional `cl =` argument mirrors what
[`optimize_ensemble_subset()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/optimize_ensemble_subset.md)
already does.

**Acceptance:** bit-identical `reff_loc` output against the serial path
(the per-member seed is deterministic, so this should hold exactly); the
existing faithfulness gate unchanged.

------------------------------------------------------------------------

## 4. Improvement 3 — per-worker shard directories

**Status:** hypothesis, unmeasured. Test before implementing. **Possible
saving:** unknown; the candidate explanation for a measured 16–33%
parallel efficiency loss.

Measured parallel efficiency through the real calibration worker
(engine + likelihood + parquet into one shared directory): **84% at k=8
on PR-123, 67% on PR-122, 75% on main.** At 80 workers on dugong the
same path sustains roughly 70 file creations/s of tmp-then-rename
traffic into a single directory.

The engine itself is embarrassingly parallel and does no I/O, so the
shared write target is the most plausible non-scaling component. Writing
to `samples/w<NN>/` per worker and globbing across subdirectories at
combine time would remove the contention if that is what it is.

**Do not implement before measuring.** The test is cheap: run the
existing `parallel/throughput` benchmark workload with per-worker
subdirectories and compare `sims_per_sec` at k = 1/4/8. If the curve
does not move, the loss is memory bandwidth or core topology and this
change buys nothing.

------------------------------------------------------------------------

## 5. Improvement 4 — `optimize_subset` collapses to its floor

**Status:** observed twice, cause unknown. **Correctness question first,
performance second.**

In the ETH 30,000-simulation run, **both** engines returned
`n_best_subset = 30` — the `min_best_subset` floor — out of ~27,000
retained draws. The grid search ran to completion and selected the
smallest permitted subset on both arms.

The consequence is visible in the metrics: `r2_cases_ensemble` was
0.1764 (main) vs 0.2994 (pr123), a 70% spread, while the `_tier`
variants computed over 113/114 draws agreed to **0.2%** (0.7841 vs
0.7855). A posterior ensemble built from 30 of 27,000 draws is dominated
by a handful of weights and is unstable; the tier metric is not.

So an expensive grid search is (a) returning a degenerate answer and (b)
producing the headline ensemble metric from it. Either the objective
genuinely prefers 30 — in which case the search could short-circuit — or
the search is broken at large N. Either way it should be understood
before it is optimised, and `optimize_subset = TRUE` should probably not
be a default until it is.

**Acceptance:** an explanation, not a speedup. Then either a documented
short-circuit or a fix.

------------------------------------------------------------------------

## 6. Improvement 5 — guard the ensemble config broadcast

**Status:** latent, not yet triggered. **Prevents an OOM; not a
speedup.**

[`calc_model_ensemble()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_ensemble.md)
`clusterExport`s the full list of sampled configs to every worker
(`R/calc_model_ensemble.R:635`). A sampled config measures **4.52 MB**
at 40 locations. At `n_subset = 1000` x 80 workers that is ~360 GB of
broadcast, before any simulation runs. The master separately allocates
`array(NA_real_, c(nL, nT, n_param_sets, n_iter))` **twice** — 4.47 GB
each at `nL=40, nT=1398, n_param=1000, n_iter=10`.

This did not bite in the runs to date only because improvement 4
collapsed the subset to 30. If the subset were large it would OOM on
hedgehog (448 GB) before producing anything.

**Fix:** compute the broadcast size up front and either refuse with a
clear message or chunk the dispatch. A guard that states the number is
worth more than one that guesses.

------------------------------------------------------------------------

## 7. Not problems (checked, recorded so they are not re-investigated)

- **No O(N^2) rescan in FIXED mode.** The adaptive loop recomputes ESS
  and weights per batch, but fixed mode dispatches all N in a single
  batch, so that cost is paid once. It would matter in adaptive mode at
  large budgets.
- **`n_iterations` amortisation is already correct** — one
  [`sample_parameters()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/sample_parameters.md)
  and one parquet write per task against `n_iterations` engine runs.
- **Thread pinning inside the calibration path is correct** — all six
  thread variables plus `ARROW_NUM_THREADS` are `"1"` in a live worker,
  set in the parent before spawn so PSOCK workers inherit them before
  `arrow` binds its pool.
- **The per-simulation [`gc()`](https://rdrr.io/r/base/gc.html) calls
  are already gone** (PR \#123), measured at 292 ms and 14.8% of the
  worker budget across the two call sites.
- **Worker memory is already much better** — median worker RSS at 40
  locations is **922 MB (PR-123)** against **1,485 MB (main/Python)**, a
  1.61x reduction measured live on dugong at 80 workers per arm.

------------------------------------------------------------------------

## 8. Sequencing

| \# | change | measured saving | effort | risk |
|----|----|----|----|----|
| 1 | chunked `open_dataset` in the combine | ~15 min / 100k run | ~1 h | low, output verifiable |
| 2 | parallelise `.mosaic_reff_resim_ci()` | ~5 min / R_eff call | ~0.5 d | low, bit-identical expected |
| 3 | measure per-worker shard dirs | unknown — measure first | ~2 h | none (measurement) |
| 4 | explain `optimize_subset` floor collapse | correctness | ~0.5 d | medium, may change results |
| 5 | guard the ensemble broadcast | prevents OOM | ~2 h | low |

Do 1 and 2 first: both are contained, both have exact acceptance tests,
and neither changes any result. Do 3 before 4 — it is a measurement, and
its answer decides whether there is anything to fix. Item 4 is the only
one that can change model output and should not be bundled with the
others.

**Each item gets its own commit and version bump**, per CLAUDE.md.

------------------------------------------------------------------------

## 9. How to measure any of this

The benchmark suite added alongside this work lives in `inst/bench/` (11
workloads, frozen fixtures, interleaved multi-arm driver, per-arm
resource sampling). Use it rather than ad-hoc timing:

``` sh
Rscript inst/bench/run_bench.R --lib=<library> --arm=<name> --src=<repo> --out=bench.csv
sh inst/bench/run_matrix.sh out.csv 4          # interleaved, multi-version
```

Two rules it enforces that matter here: **`min` is the headline
statistic**, not the mean (timing contamination is one-sided, and
min-of-5 carries 2.98% relative sd against median-of-5’s 6.22%); and
**arms are interleaved, never blocked** (an identical binary drifted
7.8% over 20 minutes on a quiet machine).

Effects below roughly **7%** are not resolvable by wall clock on this
hardware and should be measured at the call site with a 10^5-iteration
microbenchmark instead.
