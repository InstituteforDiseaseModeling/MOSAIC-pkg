---
name: iso-week-labelling-convention
description: "%Y+%V (calendar year + ISO week) vs %G+%V (ISO year + ISO week) — the surveillance + open-meteo processors use the WRONG pairing, collapsing the year-boundary week; EMDAT/IDMC use the right one. Produces 99 dup (iso,date) rows in the canonical suitability panel and contradictory LSTM targets"
metadata:
  type: reference
---

**The single most consequential unit/semantics gotcha in the weekly pipeline.**
`format(date, "%Y")` is the **calendar** year; `format(date, "%V")` is the **ISO-8601**
week. Pairing them is wrong, because a Monday in late December can belong to ISO week 1
of the *next* ISO year. `%G` is the ISO year and is the only correct partner for `%V`.

**Who does what (verified 2026-09-16, MOSAIC v0.85.0):**

| processor | pairing | correct? |
|---|---|---|
| `process_cholera_surveillance_data.R:218-219` | `%Y` + `%V` | **NO** |
| `process_open_meteo_data.R:142` (`group_by(iso_code, variable_name, year, week)`) | calendar year + ISO week | **NO** |
| `process_EMDAT_data.R:258-259` (`format(mondays + 3L, "%G")` / `"%V"`) | `%G` + `%V` | yes |
| `process_IDMC_data.R:194-195` | `%G` + `%V` | yes |

The `+ 3L` in the EMDAT/IDMC form is the idiom: take the **Thursday** of the week (Monday
+ 3) so `%G`/`%V` are unambiguous. Copy that form.

**Consequences measured on the real 2026-07-09 data (local laptop):**
- 25 Mondays mislabelled over 1973-2025; **12 inside the 2000+ compile window**
  (2001-12-31, 2002-12-30, 2003-12-29, 2007-12-31, 2008-12-29, 2012-12-31, 2013-12-30,
  2014-12-29, 2018-12-31, 2019-12-30, 2024-12-30, 2025-12-29) x 40 ISOs = **480
  country-weeks**. 5 of the 12 collide with a genuine `(N, W01)` row -> hard duplicate.
- `cholera_country_weekly_suitability_data.csv` (the CANONICAL panel `est_suitability()`
  reads): **99 duplicated `(iso_code, date)` rows**. The v7.4 panel: **200**.
- The mislabelled row is then **date-stamped ~51 weeks early**, because
  `compile_suitability_data()` rebuilds `date <- ISOweek2date(paste0(year,"-W",week,"-4"))`
  and interprets the calendar-year label as an ISO year. Example: AGO ISO-2008-W01
  surveillance (`date_start = 2007-12-31`, 263.16 cases) lands on `date = 2007-01-04`
  next to the genuine `(2007,1)` row (742.23 cases).
- **The `week >= 1 & week <= 52` filter does NOT catch it.** The long comment in
  `compile_suitability_data.R:272-293` reasons about "spurious `(year=N+1, week=53)` rows"
  and accepts a documented cost — but the real artefact is labelled **week 1** (and, in the
  climate panel, week 52). The mitigation is aimed at the wrong label.
- Row-based `slide_dbl`/`lag` walk over the duplicate: `precip_sum_12w` wrong on 3.16% of
  rows (max 152 mm), `precipitation_sum_lag12` 3.52%, **`ENSO34_lag36` (a v7.3 feature)
  11.73%**. The two copies of one cell disagree on `emdat_flood_prob` by up to **0.18**.
- Reaches the model input layer: `model/input/data_psi_suitability.csv` has 66 dup
  `(iso,date)`, **21 disagreeing on `cases`** (AGO 2018-01-04 is simultaneously
  `cases=0, cases_binary=0` and `cases=162, cases_binary=1`);
  `pred_psi_suitability_{day,week}.csv` carry 41 each. `config_default`'s `psi_jt` is the
  only defended link — `make_config_default.R:497` uses `acast(..., fun.aggregate = mean)`.
- Separate symptom of the same root cause in the climate panel: ~19 rows per country are
  "weeks" with a **365-366 day** `date_start..date_stop` span (the 1-3 January days of the
  previous ISO year folded into the same `(calendar year, ISO week)` group). All survive
  `week <= 52`. Affects `precipitation_sum`, `wind_speed_10m_max` (the cyclone GAM's lead
  predictor), `spei_approx`, and biases the per-`(iso, week)` W01/W52 anomaly climatology
  for **every** year.

**Root cause is pre-existing** (`8337fed64`, 2025-07-18) — not introduced by the v0.65-v0.84
hazard work, but it corrupts the v7.4 covariates. Reported as DATA-01/DATA-02 in
`claude/review_v084/findings/DATA.md`; repro script
`claude/review_v084/scratch/DATA/06_dupweek_impact.R`.

**How to apply:** any new weekly processor MUST use `%G` + `%V` on the week's Thursday.
Any change to the surveillance or climate week labelling invalidates every frozen psi
cache and `config_default`'s `psi_jt`. Assert
`stopifnot(!any(duplicated(d[c("iso_code","date")])))` after the
`merge(cases_data, climate, by = c("iso_code","year","week"))` in
`compile_suitability_data()` — that one line would have caught this class at build time.
See [[who-field-semantics-gotchas]] and [[open-meteo-climate-horizon-provenance]].
