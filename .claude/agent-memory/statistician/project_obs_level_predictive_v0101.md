---
name: obs-level-predictive-v0101
description: v0.101.0 observation-level posterior predictive in calc_model_ensemble - central lines MUST stay engine-level (obs-NB median collapses: suite bias 1.22 -> 0.65), intervals + predictive_median obs-level, ISO-week blocks, deaths gamma-coupling; reproduces task3 coverage on the package code path
metadata:
  type: project
---

**Shipped on feat/v0101-obs (commit 33409fcf9, 2026-10-01).** Measured on the v0.100.1
national rehearsal (25 tier A/B runs) with the package code path:
- weekly cov95 median 0.865 -> 0.992;
- cov50 0.365 -> 0.698;
- tier-A coverage passes 4/15 -> 15/15;
- weekly WIS 0.896x.

These reproduce the statistician's task3 (0.992 / 0.708 / 15/15 / 0.895).

**Invariant 1: central lines come from ENGINE draws, never from observation-level draws.**
- A daily median over obs-level NB(k) draws collapses where k is small. NB median/mean is
  about 0.29 at k = 0.13 and about 0.46 at k = 0.5.
- Suite cases bias (median over A+B) under each central line: mean 1.416, engine daily
  median 1.217, obs daily median 0.649, obs weekly median 0.689. Examples: NGA 0.91 -> 0.40,
  TGO 1.91 -> 0.07, UGA 5.45 -> 0.00.
- A collapsed median would game N-BIAS-CASES, which the frozen evaluator scores on
  predicted_central.
- The mean is Rao-Blackwellised: the obs noise is mean-preserving given the member, so
  E[obs | member] = engine. The obs-draw mean carries up to 8% MC noise at about 540 members.
- The coordinator later made this a binding constraint.

**Invariant 2: a coherent WIS needs the obs-level median.**
- Obs intervals + engine median give WIS 0.961x vs 0.896x coherent.
- The ensemble therefore carries `predictive_median` (obs-level 0.5 quantile).
- The rolling CV carries `pred_median_obs`, and its WIS uses it.
- The frozen evaluator's exact path recomputes every quantile from cases_array, so it is
  coherent automatically.

**Other design facts:**
- Weekly blocks = `.nb_disp_block(dates, week_offset)`. Offset 0 holds for all 40
  config_default locations, i.e. ISO Mon-Sun. 2023-01-01 is a Sunday, so it is a 1-day
  edge block.
- Edge partial weeks are CARRIED, as in the integrated deaths likelihood. Never count weeks
  from date_start.
- Daily values use systematic-sampling apportionment: integer, within +/-1 of the share,
  unbiased, weekly sum exact.
- Deaths: G ~ Gamma(E_w/(phi-1)), then thin (Binom(r, G)) or top up (r + Pois(e(G-1))).
  This gives exactly NB1(E_w, phi) and leaves the engine deaths unchanged at phi = 1.
  Rehearsal phi spans 1 to 11.8 (MOZ); 15 of 28 have phi > 1.2.
- The likelihood floors (cases eps, deaths bg) are excluded from the predictive. They are
  scoring devices; including them would shift the predictive mean off the engine mean.
- Confidence weights are NOT part of the generative model: they are mass-preserving
  tempering weights.

**How to apply:**
- Any consumer of member trajectories must read `.mosaic_engine_array()`: medoid, R_eff
  gate, trajectories, implied CFR, optimizer selection.
- When the likelihood stream changes k (censored k), `.nb_k_cases_resolved` must stay the
  k actually scored.
- MOSAIC-OCV calls calc_model_ensemble without observation_model, so it stays engine-level.
- MOSAIC-Mozambique plot_mean_medoid.R and enrich_trajectories.R read the persisted
  cases_array and need the engine arrays.

**Red-team of integrate/v0101 @071993706 (2026-10-01):**
- The predictions CSV kept `predicted_median` = ENGINE median beside obs-level `ci_*`; there
  was no obs-median column. That was an incoherent quantile set, crossing (median > ci_2_upper) on
  ~99% of KEN/GHA days at k ~0.14.
- The frozen evaluator's no-regression rWIS reads CSV `predicted_median` (evaluate_suite.R:183,
  :392). On the rehearsal, with v0.101 k, the incoherent pairing inflates daily-sum cases WIS by
  3.7% geo-mean (BEN/UGA/NAM ~1.22). It erases the coherent gain: 1.007 vs 0.970 against
  engine intervals.
- At small k the engine median line sits above the obs 75th percentile, so the figure line is
  outside its own 50% ribbon. The `lo50` bound is 0 on every day.
- The rolling-CV medoid/best rows are re-simulated engine-only (`.rcv_simulate_config`).
  Their intervals are not obs-level, so cross-model coverage is not comparable.
- Scratch: claude/v0101_release_redteam/stat_obs_ensemble/.

**Resolved before release:** `predicted_median` carries the observation-level `predictive_median`
(10779dbb9; M-OUTPUT 1.000 in all 168 cells, swe memory prediction-csv-quantile-coherence), and the
rolling-CV medoid/best rows are re-simulated through calc_model_ensemble() with the run's observation
model (f54f136c6).

See [[v0100-rehearsal-cases-bias]], [[nb-dispersion-estimator-traps]],
[[daily-vs-weekly-cases-scoring]].
