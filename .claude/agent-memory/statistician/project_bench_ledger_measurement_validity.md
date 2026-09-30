---
name: bench-ledger-measurement-validity
description: Measured timing-noise facts for run_simulation() + why a longitudinal wall-clock ledger cannot support sub-10% cross-version speed claims (red-team of inst/bench proposal, 2026-09-14)
metadata:
  type: project
---

Red-teamed a proposed `inst/bench/` harness + append-only `ledger.csv` intended to document
engine speed-ups ACROSS package versions. Verdict: as designed it cannot support any
cross-version speed claim smaller than ~10%.

**Why:** the dominant error term is common-mode machine drift, which replication does NOT
reduce. Only interleaved (same-session) measurement of old vs new does.

**Measured on Apple M1 Max (8P+2E), R 4.5.3, PR-122 engine, default config J=40/T=1398:**
- Fixed-seed run-to-run CV (identical work): 11.96% under `pkgload::load_all`, 3.05% installed.
- Same host / same installed binary / same seed, `min` statistic drifted 0.816 -> 0.880 s
  (7.8%) over ~20 min. Median block-to-block drift inside ONE 60-rep session: 20.0%.
- Background load (12 hogs on 10 cores): min +6.8%, median +8.8%. 4 hogs with spare cores:
  min +0.4%, median +3.0%.
- `load_all` vs installed: median +4.65% (Welch p=8e-4), min +1.72%, CV 8.55% vs 3.05%.
  **Never compare a load_all number to an installed number.**
- Estimator stability (bootstrap): rel-sd of median-of-n = 6.22/4.42/3.15/1.87% at n=5/10/20/50;
  min-of-n = 2.98/1.39/0.58/0.20%. **Prefer MIN** — timing noise is one-sided (contamination
  only adds time), so min is the ML estimate of noise-free cost.
- With a TRUE effect of zero, median-of-5 vs median-of-5 gives a 95% null interval of
  [-15.7%, +18.6%]; P(apparent change > 9%) = 29.5%. min-of-5: [-8.4%, +9.0%], P = 4.2%.
- Reps per arm for 80% power (two-sample): 9% effect -> 26; 1% effect -> 2,221.

**Workload-drift trap:** `config_default`'s `date_stop` is DERIVED FROM THE PSI FORECAST
HORIZON, so T moved 1278 (v0.32.26) -> 1367 (v0.44.17) -> 1398 (v0.47.2+), i.e. +9.4% work
with zero engine change. Any benchmark built on shipped `config_default` MUST record
n_ticks / n_locations / config_version or a data refresh masquerades as a regression of
exactly the size being hunted.

**checksum blind spots:** `sum(reported_cases)+sum(reported_deaths)` compresses 55,920 ints
(2 of 28 result channels) to a scalar. Measured delta = 0 EXACTLY for patch relabelling,
time-axis reversal, and moving mass between patches — i.e. blind to transposition/
reindexing bugs, which is the exact bug class a matrix-reorientation optimization creates.
An off-by-one time shift moved it only -0.056%. Use a full digest instead
(`serialize()` to a tempfile + `tools::md5sum()` is base-R, zero new deps).

**parallel_efficiency = thr(k)/(k*thr(1)) is a hardware metric, not a package metric:**
measured 1.00/0.90/0.82/0.73/0.65/0.56 at k=1/2/4/6/8/10 with fork workers and zero
inter-worker communication. The k=1 denominator is the most turbo-boosted point on the box;
E-cores deliver ~19% of a P-core.

**Per-draw cost (calibration sims/sec):** across 12 `sample_parameters()` draws the engine
cost spread only 2.2% (CV 0.67%) despite a 3.3x epidemic-size and 20x reported-case range,
but corr(cost, epidemic size) = +0.58. The bigger sims/sec lever is the draw-FAILURE rate:
`run_MOSAIC.R` returns early when `sample_parameters()` fails, and a failed sim costs ~0.04 s
vs ~0.9 s (20x). A prior change alone can therefore move sims/sec with no engine change.

See also [[deaths-field-naming]] for the other "looks fine, is wrong" measurement trap.
