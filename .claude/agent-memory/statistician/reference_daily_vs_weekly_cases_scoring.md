---
name: daily-vs-weekly-cases-scoring
description: Daily cells at the weekly k over-count level info 7(k+M)/(7k+M) (median 5.4x) and carry 8-14x more seed noise, yet the v0.101.0 gate BLOCKED weekly as default (rWIS W/D 1.068, |log bias| 0.254 vs 0.187) -> default back to daily 2026-10-02; daily drops NA days (legacy failed-path reward)
metadata:
  type: reference
---

**Setup (v0.100.1).** All 40 surveillance series are weekly totals spread over Mon-Sun (`round(Y/7)`
blocks). `est_nb_dispersion()` estimates k on WEEKLY totals; `calc_model_likelihood()` applies that k to
every DAILY cell unchanged, scoring ONE stochastic realisation. Deaths core is already weekly
(quasi-Poisson, CFR integrated).

**Invariants.**
- Fisher information about the log level, daily NB(k_w) cells vs one weekly NB(k_w) cell: per week
  `7(k+M)/(7k+M)` (M = weekly mean). Info-weighted over the 28 national runs: median **5.4**, 5.3-6.9 for
  tier A, exactly **1.0 for k = Inf** (Poisson is additive, so daily Poisson on a downscaled week = weekly
  Poisson). Empirical cross-draw SD ratio daily/weekly: KEN 6.5 (slope 6.2), CMR 5.8 (slope 5.7).
- Daily-cell k_w/7 restores weekly information *if* days were independent NB with a common p (NB
  additivity: sum of NB(mu_d, size_d) with common p is NB(sum mu, sum size)).
- **Seed-noise:** daily score seed SD 13.1 (KEN) / 8.8 (CMR) nats vs weekly 0.9 / 1.1. Within the
  top half of a 3,500-draw pool, KEN daily-vs-weekly Spearman is **0.17** (CMR 0.91): the daily ranking
  of good draws is mostly within-week realisation noise scored against flat Y/7 observations.
- **"Divide by the downscale factor" is a no-op:** BFRS selects the top B by rank and saturates the
  weights, so a uniform 1/7 changes neither the subset nor (materially) the posterior. It is also wrong
  in the Poisson regime (ratio 1).
- Weekly scoring does NOT cure the IS degeneracy: KEN pool exact IS ESS 1.13 (daily) -> 1.47 (weekly).
  It re-balances channels: cases:deaths score-SD ratio across the pool 12.2 -> 1.9.

**Re-selection over 13 countries.** Each country takes the top 108 of its own prior draws, using the
production seeds and pools of 2,000-3,500 draws; LL reproduces to 1e-7. Weekly NB with the same k:
- lowers selected-set cases bias in 10/13 (COG 1.59 -> 1.12, LBR 1.43 -> 1.06, KEN 1.44 -> 1.15, CMR
  1.99 -> 1.59, UGA 7.7 -> 4.3);
- improves in-sample weekly WIS in 12/13 (geometric mean 0.84) and moves median PIT_total 0.25 -> 0.44;
- leaves R2 within +/-0.02.

**ZMB is the counterexample.** Bias goes 1.44 -> 1.99 and WIS 1.16x: weekly selection lowers the troughs
but picks bigger peaks, because trough depth and peak size are dynamically coupled, and NB with k ~0.5
barely penalises a 2x error on a large week (~0.1 nat). Trough over-prediction persists under every rule
(NGA 149/wk predicted vs 16 observed in its low tercile).
- Daily k >= 1 helps only where k < 1 and is not data-supported (profile k is 0.1-0.4).
- The daily 5-rerun mean path: WIS geometric mean 0.81, but CMR worse.

Harness: `MOSAIC-pkg/claude/stat_v0100/reselect.R` + `reselect_analyze.R`; rubric projection in
`project_rubric.R`.

**Sandbox profile (medoid, rho x g):** UGA's v0.100.1 production likelihood (daily, k=0.1; UGA takes the panel-trend k 0.964 since c939095fc) has its argmax at the
7.9x over-predicted level; weekly at the same k prefers ~1.2 (grid edge).

**The default is DAILY again (2026-10-02, integrate/v0101 a28f2f42b).** The weekly rule shipped as the
v0.101.0 candidate default, but the pre-registered likelihood gate BLOCKED it on B5: on KEN/ZMB/CMR/GHA with
the same data, k, intervals and seeds, weekly was worse than daily on both counts (geometric-mean cases rWIS
W/D 1.068; median |log cases bias| 0.254 vs 0.187; GATE.md in output/production/v2026-10.02-gate-W/_gate/).
So the re-selection evidence above (synthetic + 13-country re-selection) did NOT carry over to full
calibrations; ZMB was the predicted risk and went 1.24 -> 1.56. Weekly stays available and tested.
- At the daily default the old channel balance holds (cases score keeps its per-day spread), so the
  4.8x shape/deaths shift is weekly-only.
- The daily rule's failed-path treatment is the legacy one: a NA/NaN simulated day is DROPPED (slightly
  rewards the path), +Inf -> -Inf, all-NA -> -Inf (calc_log_likelihood_negbin returns NA). Only the weekly
  core returns -Inf for any non-finite day of a scored week.
- The gate's B3 (GHA deaths bias 0.978 -> D 0.615 -> W 0.584) fails in BOTH arms, so it is not the
  scoring rule and the daily default does not cure it.

The weekly rule as built (feat/v0101-lik, 2026-10-01):
- Blocks come from the one shared helper `MOSAIC:::.mosaic_week_blocks(dates, offset)`, built on
  `.nb_disp_block` and anchored on a Monday, never on date_start (config_default starts on a SUNDAY).
- Alignment is verified: config v6.0's daily Mon-Sun sums equal the processed weekly totals of its build
  source (MOSAIC-data 64b69ab) on every week of all 39 locations, 6,488 weeks. The downscale rounds
  fractional weeks with R's round(), which rounds halves to even.
- Partial edge weeks are dropped. The core needs 3 weeks of its own; shape terms keep the daily gate.
- The floor is eps relative to the mean weekly observation.
- Each week's weight is the mean of its days' weights, then made mass-preserving over the scored weeks.
- Cost: 0.64x the daily call (interleaved), but only with offsets precomputed. Cadence detection costs
  32 ms per location per call.
- In a synthetic re-selection fixture, daily-selected level was 1.47-1.70 and weekly 0.84-1.22.

**Trap.** MOSAIC-data/processed is ONE checkout shared by every worktree: a sibling data stream rewrote
the combined weekly/daily files mid-session. A test of config against processed data checks freshness,
not alignment, so compare against the build source.

See [[nb-dispersion-estimator-traps]], [[likelihood-overconcentration-invariants]] and
[[v0100-rehearsal-cases-bias]].
