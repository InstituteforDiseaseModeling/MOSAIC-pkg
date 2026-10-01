# v0.101.0 likelihood gate: weekly cases scoring + observed-only / censored-k dispersion

Status: **plan** (2026-10-01). Run on **dugong** after `feat/v0101-lik`, `feat/v0101-data` and
`feat/v0101-obs` are integrated into MOSAIC 0.101.0 (config_default v6.1 with `reported_tier`,
priors v17.1). Nothing here has been run yet.

**Question.** Does the integrated build fix the v0.100.1 rehearsal's cases level bias in the
countries where it was diagnosed, without regressing fit or deaths? And how much of any change is the
scoring rule itself, as opposed to the data, the dispersion and the intervals?

## Countries and rehearsal baselines

Baselines are from the v0.100.1 rehearsal (`~/MOSAIC/output/production/v2026-10.01`, laptop and
dugong), scored by `acceptance_v1.0.1`. Values are on the EVAL window with exact intervals.

| ISO | tier | cases bias | cases R2 | cases WIS | cov95 / cov50 | deaths bias | why it is in the gate |
|---|---|---|---|---|---|---|---|
| KEN | A | 1.416 | 0.686 | 30.8 | 0.305 / 0.132 | 0.995 | Canonical failure. Daily-vs-weekly score Spearman 0.17 in the top half; re-selection projected bias 1.44 -> 1.15. Observed-only k 0.135 -> ~0.28. |
| ZMB | A | 1.451 | 0.728 | 62.3 | 0.769 / 0.178 | 0.993 | Known counterexample: weekly re-selection gave bias 1.44 -> 1.99 and WIS 1.16x. Watches the main risk. |
| CMR | A | 1.936 | 0.912 | 16.4 | 0.967 / 0.770 | 0.954 | Collapsed k (theta 3.6e-30, reported as the 0.1 bound) now takes the panel trend, 1.41. Re-selection at the clamped k projected bias 1.99 -> 1.59. |
| GHA | B, quiet | 1.882 | 0.153 | 53.0 | 0.955 / 0.105 | 0.978 | The data stream removed 937 imputed cases. Observed-only k ~0.14 -> ~0.31. |

## Pre-flight (all must hold before launching)

1. `packageVersion("MOSAIC") == "0.101.0"` on dugong (wrapper `~/bin/r-mosaic-Rscript`), and
   `MOSAIC:::.mosaic_likelihood_impl_version() == "R/v0.101.0+weekly_cases"`.
2. `MOSAIC:::.mosaic_config_tier(MOSAIC::config_default)` returns a matrix aligned with
   `reported_cases`, so config v6.1 carries `reported_tier`.
3. The panel trend has been re-derived for v6.1:
   `testthat::test_file("tests/testthat/test-est_nb_dispersion_panel.R")` passes. That drift test
   compares `.NB_DISP_PANEL_TREND` with `.nb_disp_panel_trend_fit(config_default, 45)`, which uses
   observed weeks only. **If it fails, stop**: re-derive, commit, reinstall.
4. Dispersion dry run, with no simulation. For each country, run
   `.mosaic_resolve_nb_dispersion(get_location_config(config_default, iso), control, score_window)`
   at `burn_in_days = 45` and record cases `k`, `status`, `panel_trend` and `n_weeks_excluded`.
   - KEN and GHA must show `n_weeks_excluded > 0`.
   - If CMR, UGA or ZAF still collapse, they must show `status == "no_estimate_se_degenerate"` with
     `panel_trend == TRUE`.

## Runs (dugong, local PSOCK; see the `dugong-run` skill)

Two arms per country, run on the same build with the same seeds (sim ids 1..30000).

| arm | `control$likelihood$cases_scoring` | everything else |
|---|---|---|
| **W** | `"weekly"` (the 0.101.0 default) | the rehearsal's control |
| **D** | `"daily"` (legacy) | identical to W |

The rehearsal's control is `1_inputs/control.json` of any rehearsal run, merged through
`mosaic_control_defaults()`. Its settings:
- fixed `n_simulations = 30000`, `n_iterations = 5`, `burn_in_days = 45`;
- `ESS_param = 1000`, `ESS_best = 100`, `best_subset_weighting = "saturated"`;
- `n_iter_ensemble = 5`, `n_iter_best = 10`;
- `io$persist_ensemble_arrays = TRUE`, which the evaluator needs for exact intervals.

The central method is whatever 0.101.0 ships (the obs stream decides it); it is the same in both arms.

```r
ctl <- MOSAIC:::.mosaic_validate_and_merge_control(jsonlite::fromJSON(
  "~/MOSAIC/output/production/v2026-10.01/national/KEN/1_inputs/control.json")$control)
ctl$likelihood$cases_scoring <- "weekly"          # arm D: "daily"
ctl$predictions$central_method <- NULL            # take the 0.101.0 default in both arms
ctl$parallel <- list(enable = TRUE, n_cores = 42L, type = "PSOCK", progress = FALSE)
cfg <- MOSAIC::get_location_config(MOSAIC::config_default, iso = ISO)
pri <- MOSAIC::get_location_priors(ISO, MOSAIC::priors_default)
MOSAIC::run_MOSAIC(cfg, pri, sprintf("~/MOSAIC/output/gate/v0101_lik/%s/national/%s", ARM, ISO),
                   control = ctl)
```

Budget: 8 runs x 150,000 simulations. At the rehearsal's throughput (about 57 min per run with
4 lanes x 42 cores) that is about 2 h.

## Scoring

Use the frozen evaluator, `claude/deploy_v0100/acceptance_v1.0.1/evaluate_suite.R`. It refuses a
modified rubric. Run it in test mode on the full pull with arrays, three times:

1. **W vs rehearsal.** Make `_rehearsal_as_baseline/national/<ISO>/v2026-10.01` a symlink to the
   rehearsal run directory. Then run:
   `--suite .../W --label gate_W --only-present --pull full --arrays yes --baseline-root .../_rehearsal_as_baseline --baseline-national v2026-10.01`.
   The overlap window scores the rehearsal ensemble against the **new** observed series. So rWIS
   compares like with like, including GHA and KEN, whose surveillance changed.
2. **D vs rehearsal.** The same command with `--suite .../D`.
3. **W vs D.** Run on suite W with the baseline tree pointing at the D runs. This is the direct test
   of the scoring rule: same data, dispersion, intervals and seeds.

Also read these from each run's `3_results/summary.json` and `run.log`:
- `nb_dispersion`: cases `median_k`, `n_panel_trend` and `status_counts`;
- `n_best_subset`, `ess_best_optimized` and `ess_is_best`;
- the log line `Cases likelihood: NB on reporting-week totals ...`.

## Criteria (likelihood stream)

Each criterion compares W with the rehearsal unless stated.

**BLOCK.** Any one of these fails the gate.
- **B1.** Cases R2 (EVAL) falls by more than 0.10 in any tier-A country (KEN, ZMB, CMR).
  Re-selection moved R2 by at most +/-0.02.
- **B2.** Cases rWIS vs the rehearsal (overlap window) is above 1.25 in any of the four. This is the
  rubric's material-regression threshold.
- **B3.** Deaths bias leaves its tier band, or |log deaths bias| grows by more than 0.15:
  - the bands are A [0.67, 1.50] and GHA (B) [0.50, 2.00];
  - the deaths core is unchanged, so deaths can move only through which paths are selected.
- **B4.** Integrity. Any run that has any of:
  - `n_best_subset < 30` or `ess_best_optimized < 30`;
  - non-finite likelihoods for more than 5% of draws;
  - the wrong impl version, or no weekly-scoring log line;
  - a cases k at the 0.1 bound together with an SE below 1e-4 of it, which means a collapse was not
    routed to the panel trend.
- **B5.** W vs D: W is worse than D on **both** counts below. That would mean the scoring rule itself
  regressed:
  - geometric-mean cases rWIS(W/D) over the four is above 1.0;
  - median |log cases bias| is higher under W.

**PASS.** Merge if all of these hold.
- **P1.** Median |log cases bias| over the four (central line) falls by at least 30%. The rehearsal
  median is 0.50 (KEN 0.348, ZMB 0.372, CMR 0.661, GHA 0.632), so the target is <= 0.35.
- **P2.** KEN cases bias <= 1.25, the tier-A band edge (rehearsal 1.416, projected 1.15).
- **P3.** CMR cases bias <= 1.70 (rehearsal 1.936, projected 1.59) and CMR R2 >= 0.85 (rehearsal
  0.912).
- **P4.** Geometric-mean cases rWIS vs the rehearsal is <= 0.95 over the four. Re-selection over 13
  countries gave 0.84.
- **P5.** W vs D: cases bias is closer to 1 in at least 3 of 4, and geometric-mean rWIS(W/D) is
  below 1.

**MONITOR.** Report these; they do not gate.
- **M1. ZMB.** Report bias, high-tercile pred/obs and low-tercile pred/obs. This is where weekly
  selection picks bigger peaks; B2 still applies.
- **M2. GHA.** Attribute its change: rehearsal -> D is the data plus the dispersion; D -> W is the
  scoring rule.
- **M3. Coverage.** Report cov95 and cov50. The observation-noise interval stream owns them, so
  they enter the integrated verdict, not this one.
- **M4. Stability.** Jaccard overlap of the W and D best subsets, plus ESS_B and |B|.

## Attribution and outcomes

The decomposition: rehearsal -> D carries data + dispersion + intervals + central line, and
D -> W carries the weekly scoring alone.

| outcome | action |
|---|---|
| **PASS** | Run the full 28-country national suite on 0.101.0, then the frozen-rubric evaluation (not test mode). |
| **BLOCK on B5** | Set the default back to `cases_scoring = "daily"` (the switch exists) and keep the dispersion changes. Investigate before a re-gate. |
| **BLOCK on B1-B3 with B5 passing** | The scoring rule helps but something else regressed. Read D vs rehearsal to find it before deciding. |
| **neither PASS nor BLOCK** | MAJOR: document the result and decide with the user. |

## Known limits

- **The panel trend is a prior centre, not a measurement.** It is flat and noisy (residual SD 1.56
  on log k, slope 0.14 +/- 0.18), so a collapsed location's k (~1) is an estimate of where k sits
  across countries, not of that location.
- **The trend constants follow config_default.** They are derived from it, and the drift test forces
  a re-derivation after every rebuild (pre-flight 3).
- **Shape terms.** They are off in production. If one is enabled, its weight against the cases core
  is about 5-7x what it was under daily scoring.
- **Edge weeks.** Cases drop partial weeks at the window edges, while the integrated deaths core
  scores edge weeks on the days it has. At most two weeks differ between the channels.
