---
name: iso-week-labelling-da01-review
description: DA-01 (%Y->%G with %V) review — the complete ISO-week sibling set in MOSAIC, which guard catches which site, the open-meteo mtime-cache trap, and the W53 three-site dependency
metadata:
  type: project
---

Adversarial review of DA-01 (ISO-8601 week labelling) on `feature/psi-12wk-evolve`, 2026-09-17,
off main @ c947820a3 / v0.90.5. Verdict was REQUEST-CHANGES on three items; the labelling fix
itself is correct and verified.

**Why:** `%V` (ISO week) was paired with `%Y` (calendar year) in three processors. Only `%G` is a
valid partner. The pairing collapses the year-boundary week into the following January.

**How to apply:** use this as the map of the ISO-week sibling set whenever a week/year key changes.

## The complete sibling set (verified by grep + measurement, 2026-09-17)

Correct (`%G`+`%V`): `process_EMDAT_data.R:284-285,312-317`, `process_IDMC_data.R:208-209,232-237`
(both use the Thursday-shift idiom `format(mondays + 3L, ...)`), and after DA-01
`process_open_meteo_data.R:134-135`, `process_cholera_surveillance_data.R:222-223`,
`run_rolling_cv_suitability.R:339-340`.

Correct but *not* a join key — do not "fix": `evaluate_rolling_cv.R:341-343` is a pure week-of-year
climatology index (`tapply(observed, woy, mean)`), never paired with a year.

Still wrong / still latent:
- `process_SUPP_data.R:81,83` — `lubridate::year()` + `lubridate::epiweek()`. Same defect class.
  Measured on Mondays 2000-2027: 8 Mondays where `year() != epiyear()`, 5 collapsed `(year, week)`
  cells spanning up to 365 days. Also `epiweek()` (MMWR, Sunday-start) differs from `%V` on
  212/1461 Mondays. Latent only because `process_cholera_surveillance_data()` dedups on
  `date_start` and takes year/week from its own grid (`carry_cols` omits them).
- `est_seasonal_dynamics.R:112` — `lubridate::week()`/`lubridate::year()` then
  `merge(..., by=c("year","week","iso_code"))` at :119 against the surveillance panel. Measured on
  real MOZ files 2010-2025: **314 of 834 matched rows (37.6%) pair a surveillance week whose Monday
  lies outside the precip week's own day span**. Pre-existing, NOT a DA-01 regression (DA-01 in fact
  removes a row-duplication there). Line :122 reconstructs date as `Jan-1 + (week-1)*7`, which is
  self-consistent with `lubridate::week` — so only the cholera merge is cross-convention.
- `process_cholera_surveillance_data.R:329` — the DAILY combined CSV's `week` column is
  `lubridate::wday(date)` = day-of-week, not ISO week. No consumer found; a naming landmine.

External source conventions (verified against data, not assumed):
- ENSO (`enso_weekly.csv`, from the enso-data repo) is **already correct ISO**: 0 of 11,920 rows
  disagree with `%G`/`%V` of their `date_start`; every `date_start` is a Monday; it *does* emit W53.
- The open-meteo raw parquets carry calendar `year` — DA-01 re-derives on the R side after the
  ERA5/CMIP6 rbind, which is the right place.

## Which guard catches which site (the thing the change's own comment got wrong)

`compile_suitability_data.R:~280` `stop()` on duplicate `(iso_code, year, week)` catches the
**surveillance** defect only. Proven: replaying `process_open_meteo_data`'s weekly aggregation on
the MOZ daily parquet under `%Y`+`%V`, then compile's exact `pivot_wider(values_fn=mean)` +
`select(-date_start,-date_stop)`, gives **0 duplicate keys** — the climate side collapses Jan+Dec
into ONE row via `group_by`, producing a silently-wrong *value* (19 cells/country spanning 365-366
days), never a duplicate. All 99 panel duplicates came from the surveillance square grid, where
under `%Y` these Monday pairs collide: {2001-01-01, 2001-12-31}, {2007-01-01, 2007-12-31},
{2012-01-02, 2012-12-31}, {2018-01-01, 2018-12-31}, {2024-01-01, 2024-12-30}.

The invariant that *does* catch the climate site: every `(year, week)` group spans <= 7 days.

## The open-meteo mtime cache is a code-change trap

`process_open_meteo_data.R:72-84` skips a country when its outputs are newer than its raw sources.
A code-only correctness fix never invalidates it. Both production callers omit `force`:
`update_mosaic_data.R:226` and `model/LAUNCH{,_sanitized}.R`. Precedent: the NEWS.md soil-moisture
entry says the same thing verbatim ("the cache check compares source vs. output mtimes and will not
otherwise pick up the upstream schema change"). **Any change to this function's output semantics
needs a one-time `force = TRUE` instruction or a real cache key.**

## W53 is a three-site dependency, not a free choice

`compile_suitability_data.R:~315` `filter(week >= 1 & week <= 52)` is load-bearing for:
- `est_suitability.R:349` — `if (53 %in% d_all$week) stop("week index is out of bounds")`
- `compile_suitability_data.R:1716` — the same hard stop
- `process_WHO_weekly_data.R:72-82` — folds every W53 row into W52 with `aggregate(sum)`, so the
  cases side *structurally cannot* emit W53 (its comment claims W53 is "spurious", true for 2025,
  false for 2004/2009/2015/2020/2026).
ENSO and (post-DA-01) climate both DO emit W53. Removing the filter without fixing all three
produces climate/ENSO W53 rows with structurally-NA cases and two immediate `stop()`s.

## Blast radius shape: stale values, never a wrong join

`psi_jt` is `reshape2::acast(tmp, iso_code ~ date, ...)` (`make_config_default.R:497`) — keyed by
**date**. And `compile_suitability_data.R:321` derives `date` FROM the label via
`ISOweek2date(paste0(iso_week,"-4"))`, so label and date are mutually consistent by construction.
Consequence: every pre-DA-01 `(year, week)`-keyed artefact (config_default$psi_jt, priors_default
via `make_priors_default.R:85`, frozen psi caches, est_seasonal_dynamics products) is *value*-stale,
not *key*-misaligned. Nothing errors; a fresh build just disagrees.

Measured drift on the 37,219 shared `(iso_code, date)` rows (pre/post backup at
`claude/psi_evolve/da01_backup/panel_PRE_DA01.csv`): climate drift 88-92% inside boundary weeks
{1,2,51,52,53}; ENSO34 drift 0% at boundaries / 100% in 2026 (pure source refresh, confirming ENSO
was already ISO); `precip_sum_12w` moved on 16% of rows; duplicates 99 -> 0.

See [[reviewer-checklist]], [[rcmdcheck-baseline-v084]].
