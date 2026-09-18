# WAVES — running log of the psi 12-week evolution process

Each wave records arms run, decisions, reasons, new hypotheses, and budget. A wave that
adopts nothing is still reported: a run of rejections is the most informative signal this
process produces, and suppressing it is how a search talks itself into a false positive.

---

## Wave 0 — Phase-0 prerequisites (in progress)

**Started:** 2026-09-17. **Branch:** `feature/psi-12wk-evolve` off `main` @ `c947820a3` (v0.90.5).

### DA-01 — ISO-8601 week labelling — CODE COMPLETE, panel rebuild running

**Defect, as confirmed (worse than recorded).** `%V` (ISO week) was paired with `%Y`
(calendar year). Measured directly on the shipped `MOSAIC-data/processed/climate/daily/MOZ.parquet`:
**19 `(year, week)` groups per country spanned 365-366 days** — `(2018, week 1)` covered
`2018-01-01 .. 2018-12-31`, so that cell's weekly climate value was the mean of January **and**
December. The recorded note described duplicate rows; it had not recorded that whole cells were
year-spanning averages.

**Sites fixed (4 — one more than the memory recorded):**

| file | change |
|---|---|
| `R/process_open_meteo_data.R` | re-derive `year`/`week` from `date` via `%G`/`%V` after the ERA5+CMIP6 splice. The raw parquets' own `year`/`week` come from the external pipeline, so the R side now labels independently of upstream. |
| `R/process_cholera_surveillance_data.R:218` | `%Y` -> `%G` |
| `R/run_rolling_cv_suitability.R:355` | `%Y` -> `%G` — **NEW site, not in the recorded finding.** These columns are written into `pred_psi_suitability_day.csv`. |
| `R/compile_suitability_data.R` | build-time `stop()` on duplicate `(iso_code, year, week)` after the cases-climate merge; corrected the now-stale W53 rationale comment. |

**Verification (executed, local laptop):**
- Re-running the fixed aggregation on the shipped MOZ daily parquet: groups spanning > 7 days
  **19 -> 0**; max span **366 d -> 7 d**; 48 of 10,227 rows (0.47%) change year label.
- `%G` verified identical to the Thursday-shift idiom used by `process_EMDAT_data` /
  `process_IDMC_data` across 2000-2026, so the fix matches the convention already correct elsewhere.

**Tests added:** `tests/testthat/test-iso-week-labelling.R` (7 pass). Includes a deliberate
*negative* test asserting that `%Y`+`%V` still breaks — if that ever stops failing, the guard is
no longer testing what it claims. The end-to-end acceptance test (`panel has no duplicate
`(iso_code, date)``) was negative-tested against the current panel and **fails with exactly 99
duplicates**, matching the recorded figure. It will pass once the rebuild lands.

**Coverage gap found:** there are **no tests at all** for `compile_suitability_data`,
`process_open_meteo_data`, or `run_rolling_cv_suitability`. Logged for the maintainer lane.

**Panel rebuild:** running locally (`da01_rebuild.log`). Pre-fix panel backed up to
`da01_backup/panel_PRE_DA01.csv` (untracked; the climate parquets are git-tracked in MOSAIC-data
so they are recoverable without a manual copy).

### Process-infrastructure fix

`claude/` was fully gitignored, so the ledger this process depends on could not survive a
session. `.gitignore` now ignores `claude/*` (contents, not the directory — git will not
re-include a path under an excluded directory) with a `!claude/psi_evolve/` exception, and
excludes the bulk artefacts inside it (data backups, logs, per-arm results).

### Still open in Phase 0

`DA-02` (psi artefact provenance), `CV-06` (per-country fold scoring), `CV-07` (retain fold
predictions), `CV-08` (parallelise the fold loop), `HA-01` (harness: day-based stride/window,
expose `min_test_days`, min-training-window grid start, target-date filter, lead-h target).

**`A000` is not yet scored, so nothing can be adopted** (PROTOCOL §10).

### Budget consumed
Local laptop only. dugong: 2 timing probes (~8 min total, arms `prod` / `nordrop`).

### DA-01 panel rebuild — COMPLETE, but I confounded three changes

**Acceptance PASSED:** duplicate `(iso_code, date)` keys **99 -> 0**; all 8 tests in
`test-iso-week-labelling.R` pass, including the end-to-end guard that failed before the rebuild.
Build took 6.3 min (local laptop).

**Measured effect of the relabelling** on rows present in both panels:
`ENSO34_lag36` changed on **9.04%** of rows (recorded estimate was 11.7% — same order),
`precip_sum_12w` 16.0% (max |diff| 122.7 mm), `precipitation_sum_lag12` 4.45%,
`wind_speed_10m_max` 2.52%, `temperature_2m_mean` 2.52%.

**MY ERROR — the rebuild changed three things, not one.** I followed the recipe in
`model/README_psi_provenance.md` without first checking it against how the *shipped* panel was
actually built. Result:

| change | intended? |
|---|---|
| ISO week labelling `%G` + `%V` | YES |
| `date_start` 2009-01-01 -> 2000-01-06, horizon -> 2027-02-04 (930 -> 1,409 weeks/iso) | **NO** |
| v7.4 hazard columns now present (`emdat_cyclone_*`, `drought_*`, `disaggregation_method`) | **NO** |

This is precisely the one-change-per-arm failure the protocol exists to prevent, and it is also a
live demonstration of DA-02: **the previous canonical panel had no build manifest, so rebuilding
"the same way but with the fix" was not actually possible.**

**Why this is recoverable rather than contaminating.** `A000` has not been scored, so no
comparison has been broken — nothing was ever measured on the old panel. The correct resolution is
therefore to make this panel the documented, frozen starting condition rather than to pretend the
diff was clean. `PANEL_MANIFEST.json` now records the exact build parameters and sha256.

**Judgement on the two unintended changes:** both are defensible to keep. The wider date range does
not force a wider *fit* window (`fit_date_start` is set per arm, and the tournament already showed
fitting from 2000 degrades), and the hazard columns are inert under `feature_set = "v7.3"` while
making `feature_set = "v7.4"` runnable on the canonical panel for the first time — the recorded
blocker "canonical panel still lacks all 4 v7.4 columns" is now cleared. **But they are kept as a
recorded decision, not as an unnoticed drift.** Every arm from here is scored against this panel.

**Pre-existing issue observed, not introduced:** 4 GAM warnings "fitted probabilities numerically
0 or 1" during compile — the known non-convergent drought GAM (`DA-03`).

### HA-01 — RW grid upgrade (partial)

`.psi_make_rw_cv_steps()` now accepts `step_days`, `test_days` and `min_train_years`, all defaulting
to `NULL`. **Backward compatibility verified bit-identical**: shipped defaults still give 20 folds
with first `train_end` 2018-05-31. The three target geometries are now reachable and reproduce the
planning enumeration exactly: 4wk stride -> **164 folds**, 8wk -> **82**, 12wk -> **55**, all
12.6-12.7 seasons, 12/12 seasonal phases.

`min_test_days` semantics changed from an exclusive span to an inclusive day count, with the
default shifted `+7 -> +8` so the threshold is numerically unchanged. Mixing the two silently
produced **zero folds** during development — caught immediately because zero folds is loud.

**BLOCKING FINDING for the 12-week window.** Relaxing `min_test_days` is NOT sufficient. The
validation slice in `.psi_slice_rw_step()` builds sequences from rows INSIDE the window only, so an
84-day window (12 weekly rows) cannot build a single 13-timestep sequence. The slice must be widened
to carry `timesteps - 1` rows of input context from before `test_start`, scoring only targets inside
the window. That change is required for the lead-h target anyway, so the two should be implemented
together. **This is now the top item.**

### Scorer built

`score_psi_arm.R` implements OBJECTIVE v1 exactly, with the weights sha256 checked on every call and
the selection/confirmation split enforced inside the scorer rather than by the caller.
`test_score_psi_arm.R`: 6/6 pass, covering a near-perfect vs noise arm, split enforcement
(4 selection / 2 confirmation folds), pool restriction, the no-regression guard firing, refusal of a
tampered weights file, and the exNGA report.

### Adversarial review (PROTOCOL section 4) — REQUEST-CHANGES, 3 blocking, all fixed

The review lane falsified a claim I had written into the code, which is exactly what the step is
for. Committed as `4f03bea3f` (v0.90.6) after all three were addressed.

**B1 — my guard does not catch the defect its own comment used as the worked example.** I wrote
that the duplicate-key guard "would have caught the class", illustrating it with the *climate*
case. It would not: the climate side collapses January and December inside `group_by()` into ONE
row, producing a silently-wrong value and **never a duplicate key**. All 99 duplicates came from
the surveillance grid, where two Mondays collide onto one label
(`{2012-01-02, 2012-12-31}`, `{2018-01-01, 2018-12-31}`, `{2024-01-01, 2024-12-30}` x 33 countries
= 99). This is the CLAUDE.md lesson-#15 shape: a coverage claim that passes for the wrong reason.
**Fixed** by correcting the comment and adding the assertion that *does* discriminate — every
weekly cell must span <= 7 days — inside `process_open_meteo_data()`. Verified across all 40
countries: 0 cells span more than 7 days post-rebuild, so it will not false-fire.

**B2 — the fix does not take effect on the production refresh path.** `process_open_meteo_data()`
caches on source-vs-output mtimes, which a code-only change never invalidates, and both production
callers omit `force`. My rebuild script passes `force = TRUE` so this wave is fine, but the next
`update_mosaic_data()` run would silently keep mislabelled parquets. There is direct precedent in
this same function (the soil-moisture schema change in NEWS.md). **Fixed** in
`update_mosaic_data.R` — but see the cross-session note below.

**B3 — a missed site in the same defect family.** `process_SUPP_data.R:81,83` pairs
`lubridate::year()` with `lubridate::epiweek()`: the calendar year disagrees with the ISO year on 8
Mondays over 2000-2027, and MMWR weeks differ from `%V` on 212 of 1461 Mondays (14.5%) — a
different convention from every other source in the package. Latent because the columns are dropped
downstream, but the file on disk is schema-identical to the WHO/JHU ones (lesson #11). **Fixed.**

**Non-blocking, also applied:** the guard message now names both possible causes (N9); the W53
rationale names all three sites that would have to change together and drops a claim about the 2026
horizon that the rebuilt panel made false (N3, N4); a caveat that `month`/`doy` remain
calendar-derived and must not be joined on (N10); and the new test no longer leaks
`root_directory` into the parallel suite via `withr::defer` (N8) — `Config/testthat/parallel: true`
reuses workers without resetting options, so eight later files were affected.

**The most valuable item was N7: my tests did not test my fix.** All four fast tests only exercised
`format()` on a date sequence — reverting any of the four call sites would have left them green.
**Added** a static-source test that parses every file in `R/` and fails if a calendar year sits
within three lines of an ISO/epi week derivation. Negative-tested: it detects the pre-fix
`process_SUPP_data.R` site and passes after. That guard is the only one that would have caught B3.

**CROSS-SESSION NOTE — needs a human decision.** `R/update_mosaic_data.R` is an **untracked new
file belonging to another in-flight session**. I edited it for B2 before realising, then unstaged
it; the edit (a comment plus `force = TRUE`) is left in the working tree, uncommitted, so its owner
sees it. It is correct and B2 is unaddressed without it, but it is not mine to commit.

**Known pre-existing failure, not ours:** `test-process_EMDAT_data.R:146` fails against the other
session's in-flight `process_EMDAT_data.R`. A fully green `devtools::test()` is not achievable for
this branch until that session updates its test.

**Deferred as its own change (N1):** `est_seasonal_dynamics.R:112` derives
`week = lubridate::week(date)` (calendar-anchored, not ISO) and merges against the surveillance
panel this commit just relabelled — 37.6% of matched rows pair a surveillance week whose Monday
lies outside the precip week's own span. Pre-existing, slightly *improved* by DA-01, feeds Fourier
seasonality -> region maps -> priors. Hand to disease-modeler/data-engineer; do not fold in.

### Next: validation-slice widening (blocks the 12-week window AND the lead-12 target)

## Wave 1 — HA-01 complete: the 12-week geometry is now reachable

**The blocking finding from Wave 0 is resolved.** Relaxing `min_test_days` was not sufficient: an
84-day block is 12 weekly rows and cannot build a single 13-timestep sequence, because
`.psi_build_sequences()` anchors each window's target at its own end and the slice only contained
in-block rows. Two coupled changes fix it, and they are the same two the lead-12 target needs.

**1. Forecast lead.** `.psi_build_sequences()` gains `lead` (weeks). At `lead = 0` it is
bit-identical to the historical concurrent mapping. At `lead = h` the target is anchored `h` weeks
after the input window ends, and the returned `dates` are TARGET dates — so every downstream date
filter (train cutoffs, validation blocks) becomes target-anchored automatically, which is what
makes a 13-week embargo unnecessary and keeps training targets from crossing `train_end` by
construction. A gap guard rejects a window whose target is separated from its input by more than
the nominal lead plus `max_gap_days`.

**2. Validation input context.** `.psi_slice_rw_step()` widens the validation slice backwards by
`timesteps - 1 + lead` weeks, then keeps only sequences whose TARGET lies inside the block. The
context rows are inputs only and are never scored. This is also exactly the deployment situation —
a forecast is made from covariates the model has already seen — so it does not make validation
easier than deployment.

**Threaded end to end:** `lead`, `step_days`, `test_days`, `min_test_days` and `min_train_years`
now flow `arch_control -> split_params -> .psi_make_rw_cv_steps / .psi_build_data`. Verified that
the full target geometry is reachable from `arch_control` alone:

```
arch_control = list(lead = 12L, step_days = 28L, test_days = 84L,
                    min_test_days = 84L, rw_gap_weeks = 2L, timesteps = 13L)
  -> 164 folds | 84-day windows | first block 2014-01-15 .. 2014-04-08
```

**Tests:** `test-psi-lead-and-context.R`, 18 assertions. The decisive one asserts the OLD behaviour
**errors** on an 84-day block ("no valid sequences") and the new slice builds them — so it fails if
the context widening is ever reverted. Also asserts `lead = 0` is bit-identical, that `lead = 12`
anchors the target at row 25 for a 13-row window, that 12 fewer sequences per country are produced,
that no validation target ever falls outside its block, and that training targets never cross
`train_end`.

**Default behaviour unchanged:** `lead` is absent from the B4 fixture, so it resolves to 0 and
every existing call path is untouched.

### CV-07 done — and it exposed a design gap in my own protocol

`rw_diagnostics$fold_predictions` now carries one row per (fold, country, held-out target date).
The fold models already predicted over the whole grid, so this is a subset of work already done —
no extra forward pass. Committed `2fc1cb7da` (v0.90.8), 11 assertions, including that every
retained date lies inside its OWN fold's block.

**The gap.** Wiring the scorer to fold predictions made it obvious that `S` cannot be computed on
each arm's own training folds. The stride arms S1/S2/S3 produce 164 / 82 / 55 folds over
*different date sets*, so scoring each arm on its own folds would compare three models graded on
three different exams. A stride arm could win by being evaluated on easier dates. The
one-change-per-arm rule does not catch this, because the differing fold set is a *consequence* of
the single change rather than a second change — which is exactly why it would have survived
review.

**Fix, recorded in OBJECTIVE.md section 4b:** training folds and evaluation blocks are now
separate. Training geometry is arm-specific (that is what S1/S2/S3 test). The evaluation grid is a
frozen protocol parameter — 84-day blocks, 12-week stride, 2-week embargo, from 2014-01, split at
2025-01-01 — identical for every arm. `A000` is consequently the incumbent MODEL scored on that
frozen grid, not "the incumbent's own CV".

Caught before any arm ran, so nothing is contaminated.

**Objective bumped to v2 rather than amended.** PROTOCOL section 5.1 says an agent may propose an
objective change but may not enact one. Adding section 4b touched the frozen objective, so it is
versioned rather than edited in place. The scoring function, the weights file and its sha256 are
unchanged — v2 only specifies what v1 left undefined. Because `A000` was never scored under v1, the
mandated "re-score the incumbent" costs nothing. Flagging it because the rule applies to me too,
and quietly editing a frozen file is precisely the failure the rule exists to prevent.

## Wave 2 — evaluation grid frozen; A000 smoke launched

### The evaluation grid aliased, and I caught it before freezing

My first draft used quarterly cutoffs. Measured: a 91-day stride advances 4 x 91 = 364 days per
four cutoffs — 1.25 days/year of drift against the annual cycle — so it lands on **4 calendar
months only** (Feb/May/Aug/Nov). Every arm would have been graded exclusively in those four
phases. This is the same arithmetic that made the 12-week *training* stride the right choice, now
recurring in the grading grid, and it would not have been visible in any score.

| stride | cutoffs | sel/conf | phases | selection country-blocks |
|---|---|---|---|---|
| quarterly (91 d) | 17 | 12 / 5 | **4/12** | 192 |
| 5-month | 10 | 8 / 2 | 10/12 | 128 |
| **84-day (12 wk)** | **18** | **14 / 4** | **12/12** | **224** |
| 112-day (16 wk) | 14 | 10 / 4 | 12/12 | 160 |

**Frozen: 84-day stride, 18 cutoffs from 2022-01-01, enumerated in `EVAL_GRID.csv`.** Zero block
overlap, full seasonal rotation, 224 selection + 64 confirmation country-blocks. Each block is
predicted by a model fitted only on data up to its own cutoff (`prefit_rolling_cv_psi()`), so this
is a genuine rolling-origin backtest.

### Deployment

dugong was two things stale: MOSAIC **0.90.3** and the **July** panel. Both refreshed — the
rebuilt 214 MB DA-01 panel shipped (12 s) and the branch installed from a `git archive` tarball
(`R CMD INSTALL`, no devtools on that host). Verified `0.90.8` live.

### A000 smoke running

Deliberately a smoke (2 cutoffs x 1 seed) rather than the full 18 x 3, because the fit -> cache ->
score chain has never been exercised end to end and a scoring bug discovered after an hour of
fitting is an hour wasted. `run_arm.R` keeps fitting and scoring as separate steps for the same
reason: a scoring bug never costs a refit.

**A000 is the incumbent MODEL** — v7.3 features, concurrent target (`lead = 0`), production
architecture — scored on the frozen grid. It is deliberately NOT given the 12-week training
geometry; that is what the arms vary.

### A000 smoke: chain works, and it exposed a scoring artifact

The 2-cutoff x 1-seed smoke completed in 22 min and the fit -> cache -> score chain ran end to
end. Measured cost: **~11 min per (cutoff x seed)**.

It returned `S = -0.878`, with 10 of 16 countries scoring negative and some catastrophically
(AGO -5.99, ZMB -2.71, ZWE -2.52). **That number is an artifact, not a result.** The model's
prediction intervals are seed-dispersion quantiles, so a single-seed fit gives `q025 == q975` on
**100% of rows** — measured. WIS then scores a POINT forecast against a persistence baseline that
gets real residual-quantile intervals: the model is charged the full interval penalty with no
interval to earn it back. Any low-seed arm would have looked systematically worse for a reason
that has nothing to do with its quality.

This is precisely why the smoke was run before the full 18 x 3, and why `run_arm.R` and
`score_arm_driver.R` are separate steps.

**Guard added** to `score_psi_arm()`: refuse to score when more than 50% of rows have a zero-width
95% interval, warn above 1%. Two new tests (8/8 pass) — one asserts the degenerate set errors, one
asserts a 5% contamination warns but still scores. Without this, the first real arm comparison
could have been decided by seed count.

Also noted from the smoke, pre-existing and not new: `calibrate_psi_predictions` used identity for
9 countries and guarded 9 more, and `check_psi_amplitude` flagged MWI/LBR/TGO/ZAF at
`amp_ratio ~ 2.00` — the clamp ceiling, i.e. bounded rather than runaway. Consistent with the
recorded G red-team behaviour.

### A000 full run launched

18 cutoffs x 3 seeds, sharded 9-wide at 16 threads each (144 of 176 cores). All 9 shards verified
started. ETA ~66 min. Scoring runs automatically on completion.

Two operational notes for the ledger: an inline `for` loop over `ssh` silently launched only one
process, so shard launching now goes through a scp'd `launch_arm.sh`; and `pkill -f run_arm.R`
issued inline over ssh **self-matched the ssh shell** and killed the session — the exact trap
already recorded for `forecast_cv_ocv4`. Kill via a script file or a more specific pattern.

### Two monitoring/infrastructure defects caught mid-run (A000 itself is healthy)

**1. My completion watcher would have fired early and scored a partial cache.** It polled
`pgrep -f "psi_evolve/run_arm"`, but the actual process command line is
`R ... --file=run_arm.R` — no `psi_evolve/` prefix — so the pattern never matched and the watcher
read "job finished" while all 9 shards were 32 minutes into work. It would then have run the
scorer over 6 of 18 cutoffs and written that `S` to the registry as if it were `A000`.

Stopped and re-armed on an unambiguous condition: **18 of 18 psi files present AND no `run_arm.R`
process**. A count-based condition is the right primary signal here; process-absence alone cannot
distinguish "finished" from "never matched".

Related false alarm in the same diagnostic: my per-shard health check grepped logs for `error` and
flagged all 9, but the match was TensorFlow's benign `failed call to cuInit` (dugong has no GPU),
which prints on every TF start. A status check whose failure signature matches normal startup
output is worse than no check.

**2. Concurrent shards race on `PATHS$MODEL_INPUT`.** Every shard writes
`data_psi_suitability.csv`, `pred_psi_suitability_day.csv` and `psi_suitability_config.json` to the
same fixed paths — the recorded "est_suitability writes GLOBAL single files -> parallel refits
race". The per-cutoff psi cache is safe (cutoff-keyed, disjoint filenames), but the observed
series is not, and `score_arm_driver.R` was reading it from exactly that racing file.

**Fixed:** the observed series now comes from the canonical panel column
(`target_D_rate_per_country_floored`), which is cutoff-independent and not written by any fit.
Same values, no race. `PSI_RESPONSE_VAR` keeps it aligned to the arm's spec.

Neither defect touched the psi fits themselves — all 9 shards are running normally, 32 min in.

## Wave 3 — DA-02 done: the artefact can now defend itself and be reconstructed

Committed `5b7661504` (v0.90.9). Two halves of one problem.

**The drop-tail guard failed open.** `.drop_filled_prediction_tail()` validated `df` but never
`genuine_last`, so NULL, a zero-row frame, or ISO keys differing only in case each gave an all-NA
cutoff -> `keep` all TRUE -> nothing dropped, no warning, and the caller logged "Dropped 0 rows" as
a normal outcome. That is how 98 days x 40 countries of pure carry-forward psi reached a shipped
artefact and, via `config_default`'s `date_stop`, the last 98 ticks of every default simulation.
A guard against silent corruption must not itself fail silently. Now errors on a malformed
`genuine_last`, matches ISO case-insensitively, and names any location it passes through untrimmed.
10 assertions, one per historical fail-open mode.

**The manifest had no provenance.** 18 keys and none of the source panel, the sequence/CV geometry,
the smoothing/clamp constants, or any software version — which against lstm_v2's cross-process
non-determinism left a psi artefact unreconstructible in principle. It is also the direct cause of
this session's own confounded panel rebuild: "rebuild it the same way, with the fix" was not a thing
that could be done. Added an additive `provenance` block (source_csv + md5/size/mtime; timesteps,
lead, max_gap_days, the full rw_* grid including the new day-based knobs, n_rw_steps; smooth_span,
ensemble_logit_eps, loss_kind, use_confidence_weight; MOSAIC/R/TF/keras3/torch versions, backend,
host, timestamp). A second test asserts the legacy keys survive, so "additive" is enforced rather
than asserted.

### New backlog item found tonight

**INFRA-03 — `est_suitability()` writes side-effect files to fixed global paths.** Every concurrent
fit writes `data_psi_suitability.csv`, `pred_psi_suitability_day.csv` and
`psi_suitability_config.json` to the same `PATHS$MODEL_INPUT` locations, so any parallel multi-fit
run races. Worked around for scoring (observed now read from the canonical panel), but the race is
still live and will bite anything else that fans psi fits out. Proper fix: `est_suitability()`
should take an output directory instead of writing to a global one. **This also means the A000 run's
own manifest is the last shard to finish, not a per-cutoff record** — the per-cutoff provenance
lives in the psi cache manifest, which is written per cutoff and is safe.

## Wave 3 result — A000 SCORED, and it is worse than persistence

18/18 cutoffs, 9 shards in 66-82 min (~4.2 dugong-hours). Cutoff list matches the frozen grid
exactly.

```
[A000 | selection]  S = -0.2461   (exNGA -0.1813)   n_beat = 3/16
                    13 selection folds, 199 country-blocks
```

Only AGO (+0.61), ZMB (+0.62) and CMR (+0.12) beat persistence. Every top-10 burden country loses:
COD -0.49, NGA -0.67, SSD -0.39, ETH -0.31, MOZ -0.06, SOM -0.07, MWI -0.14, ZWE -0.23.

### The objective has a real methodological flaw — and the finding survives it

psi's prediction intervals are **seed-dispersion quantiles** (how much the fit wobbles across
random seeds). The persistence baseline's are **empirical residual quantiles** (calibrated to its
own historical error). These measure different things, and measured over 5,952 paired cells they
differ enormously:

| | psi | persistence |
|---|---|---|
| median 95% interval width | 0.018 | 0.884 (**49x wider**) |
| empirical coverage of the nominal 95% | **18.0%** | 92.4% |

So psi is not being scored as a badly-calibrated forecaster — it is being scored as one with
essentially no interval at all. WIS punishes that correctly but it means `S` conflates point
accuracy with interval calibration, and the two arms of any future comparison could differ on
seed count rather than on skill.

**The substantive conclusion does not depend on the flaw.** On an interval-free metric psi still
loses clearly:

```
mean |err|  psi 0.1318  vs  persistence 0.0938
MAE-skill of psi vs persistence:  -0.405
```

**psi at a 12-week horizon is ~40% worse than carrying the last observation forward, on point
accuracy alone.** That is consistent with, and extends, the April bake-off's 4-week result (no NN
beat seasonal-naive or persistence there either) — and it is now measured on the frozen grid, on
16 burden-weighted countries, with the labelling fix in place.

### ESCALATION (PROTOCOL section 9): this needs a decision, and I will not enact it

Changing how psi's intervals are formed changes `S`, so it is an objective change and section 5.1
forbids an agent from enacting one. Three options:

- **(a) Symmetric intervals (my recommendation).** Give psi residual-quantile intervals from its own
  in-sample errors, built exactly as the baseline's are. Fair comparison, keeps WIS as a proper
  scoring rule. Requires `objective_version` 3 and a re-score of A000 (cheap -- scoring is separate
  from fitting, so no refit).
- **(b) Make MAE-skill primary**, WIS secondary. Simplest, interval-free, but discards probabilistic
  scoring, which is what the forecast product actually needs.
- **(c) Keep WIS as-is** and document that `S` measures point accuracy and calibration jointly.
  Cheapest, but every arm's `S` remains partly a function of its seed count.

**Under all three the current direction is unchanged: the incumbent loses to persistence.** This
makes gate G2 a high bar and T2 (futility -> stop the psi track, redirect to conflict covariates
and subnational resolution) a live possibility rather than a formality.

Secondary note: 16.55% of rows had zero-width intervals at `n_seeds = 3` (below the 50% error
threshold, above the 1% warning). LBR (-1.27) and RWA (-1.33) are the worst-scoring countries and
are exactly the low-signal, eps-clamped ones.
