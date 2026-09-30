# MOSAIC benchmark suite

Eleven workloads that measure MOSAIC's simulation and calibration speed, run
**manually**, designed to be compared across package versions over time.

Not a test suite. Nothing here asserts; everything here records.

## Running

```sh
# one arm
Rscript inst/bench/run_bench.R --lib=<library> --arm=<name> --src=<repo> --out=bench.csv

# all three engine generations, interleaved
sh inst/bench/run_matrix.sh out.csv 4

# add the expensive workloads
Rscript inst/bench/run_bench.R --lib=... --arm=... --out=... --parallel --calib
```

`--lib` is the library holding the MOSAIC being measured. This script is run
**standalone against that library**, not sourced from inside it: the suite has
to produce comparable rows from codebases whose entry points differ
(`run_LASER()` before the R engine, `run_simulation()` after), so the newest
copy of the script drives and older libraries are the subject. `compat.R`
dispatches on which entry point exists.

## The workloads

| id | measures | needs |
|---|---|---|
| `machine/calibrator` | frozen `rbinom`/`rpois`, zero MOSAIC code | — |
| `engine/full` | J=40, T=1398, config as a list — the anchor | — |
| `engine/full-path` | same, config as a file path | — |
| `engine/single-loc` | J=1 (MOZ), T=1398 | — |
| `engine/short-window` | J=40, T=400 | — |
| `engine/high-vacc` | J=40, T=400, `nu_2_jt` non-zero | — |
| `component/sample-params` | `sample_parameters()`, one draw | root |
| `component/likelihood` | `calc_model_likelihood()` | — |
| `worker/per-sim` | sample → engine → likelihood → parquet (+`gc()` where present) | root |
| `parallel/throughput` | the worker task at k ∈ {1,4,8}, N = 8k | root, `--parallel` |
| `calib/fixed-small` | a fully pinned `run_MOSAIC()` | root, `--calib` |

"root" means `options(root_directory)` and the `~/MOSAIC` data tree.

## Five decisions that make the numbers mean something

**`min` is the headline, not the mean.** Timing contamination is one-sided —
interference only adds time — so the minimum is the maximum-likelihood estimate
of the noise-free cost. Measured, min-of-5 carries 2.98% relative sd against
median-of-5's 6.22%. Mean and median are recorded too, and so is the full
replicate vector, because two order statistics and a count cannot yield a
confidence interval.

**Arms are interleaved, never blocked.** Repeated measurement of an
*identical binary* drifted 7.8% over 20 minutes on a quiet machine. Running all
of arm A then all of arm B charges that drift entirely to the arm difference.

**The workload is frozen, not inherited.** `config_default` is rebuilt from
live surveillance data and its window has already moved 1278 → 1367 → 1398
ticks across releases — a +9.4% change in work volume with no engine change,
which a wall-clock ledger cannot distinguish from a real regression. The
derived fixtures in `fixtures/` are generated once by `make_fixtures.R` and
then left alone; `config_md5` is recorded on every row.

**A machine calibrator runs every session.** Version-independent by
construction, so every row carries the host's contemporaneous speed and a row
taken today stays comparable to one taken in six months on a differently-loaded
machine. Without it, "compare within host only" is necessary but not
sufficient.

**Correctness is recorded alongside speed.** Engine workloads emit an md5 over
the serialized `reported_cases`/`reported_deaths` plus their sums. The digest is
an exact within-engine check — v0.69 and v0.71 must be bit-identical, and the
replay fixtures already prove they are. Across engine generations it is *not*
comparable: the Python and R engines agree statistically but explicitly not
draw-for-draw, so use the sums with a ±2% band there.

## What is deliberately absent

Effects below roughly 7% are not tracked. The measured wall-clock noise floor
is 3–12% depending on load and modality; a 1% row recorded at false precision
is worse than no row. Measure those at the call site with a 10⁵-iteration
microbenchmark instead.

`calc_Reff()` (fully serial, ~1,000 sequential engine runs at defaults),
`optimize_ensemble_subset()`, `est_suitability()` and plotting are all real
costs and all absent. The first three are legitimate future additions;
`est_suitability()` is not, because its LSTM training is not reproducible
across processes and so cannot produce a stable timing row in principle.

## Regenerating fixtures

```sh
Rscript inst/bench/make_fixtures.R <lib.loc>
```

Changes the workload. Every row recorded before the change becomes
incomparable to every row after it. Do this only with a deliberate reason and
note it in the ledger's `notes` column.
