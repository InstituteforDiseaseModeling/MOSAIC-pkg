---
name: surveillance-curation-table
description: inst/extdata/surveillance_curation.csv = the ONE documented table of hand corrections (who_window / drop_imputed / flag_imputed) with evidence+source, plus `shape` column + surveillance_curation_shapes.csv cumulative anchors (ZAF 2023 epicurve); date->WHO-week mapping (Monday on/before; Sun 2 Mar 2025 = 2025-W09); package curation applies in every combiner/WHO run, so test fixtures must avoid curated iso-years
metadata:
  type: reference
---

Created on feat/v0101-data (2026-10-01). Read by `.surveillance_curation()`
(R/surveillance_curation.R); validated on read (unique id, known action, evidence +
reference required, `shape` only "cumulative" and only on who_window rows).

- `who_window` (process_WHO_weekly_data): report = (report_year, report_week) as on the WHO
  dashboard; window from the WHO week containing `date_start`, cases up to the week
  containing `date_stop` (blank = report week), 0 after. Rows get
  `disaggregation_method = "who_catchup_curated"` + `catchup_curation_id`. Errors if the
  window leaves the report's epi year or covers a non-silent week; warns if the report is
  zero/missing; silent skip if the week is outside the series. Entries: ZAF-2023-AAR,
  NAM-2025-first-case, CIV-2025-W33, NGA-2023-W52.
- **Shaped windows (fix/v0101-trust 1cd75b0fd, revised a50d44c37):** `shape = "cumulative"` ->
  anchors in `inst/extdata/surveillance_curation_shapes.csv` (id, date, cumulative_cases,
  optional cumulative_deaths, note; counts through END of date; a row may carry either). Each
  WHO week (Mon-Sun) gets C(Sun) - C(prev Sun), linear between anchors
  (`.curated_shape_weights(..., value)`), rescaled to the report; whole counts via
  `.spread_count` (half up since a50d44c37); curve inside the active weeks AND total within
  0.5-1.02x the report (`.check_curve_total`), else stop. No deaths curve -> deaths follow cases. Method
  `who_catchup_curated_shaped` (tier 2), cw 0.9 (`.SHAPED_WINDOW_CONFIDENCE`, shared with
  the combiner's who_catchup_shaped); the combiner never donor-reshapes it. Only
  ZAF-2023-AAR so far ([[zaf-2023-epicurve-provenance]]). `.who_reallocate_catchup_reports`
  errors if a shaped row has no anchors passed (`shapes=` arg).
- `drop_imputed` / `flag_imputed` (combiner, before R3): tier-3 weeks lying WHOLLY inside
  [date_start, date_stop]. Entries: SSD-2023-2024-absence (2023-05-17..2024-09-27; the
  2024-09-23 week holding the first case stays), AGO-2023-absence, BFA-2025-unconfirmed
  (flag: listed unchanged in the adjustments log).

Date mapping: WHO weeks run MONDAY-SUNDAY, stamped with their Monday (MMWR-numbered only), so a
date maps to the Monday on/before it (`.who_week_of_date`). Sun 2 Mar 2025 is 2025-W09 (stamp
02-24); Wed 1 Feb 2023 = W5; Mon 31 Jul 2023 = W31. Shape anchors for weekly data go on Sundays.
(Round 1 wrongly used Sun-Sat; see [[who-weekly-w53-quirk]].)

Gotcha: the package table applies to every real AND test run, so synthetic fixtures using
ZAF 2023, NAM 2025, CIV 2025, NGA 2023, SSD 2023-24, AGO 2023 or BFA 2025 pick up the
corrections (an SSD 2023 gap-rule fixture got 14 rows curated away) -- use non-curated isos
for rule tests. Related: [[who-multiweek-catchup-reports]], [[ai-fourier-full-total-double-count]].
