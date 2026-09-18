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
