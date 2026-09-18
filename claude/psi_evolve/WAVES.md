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
