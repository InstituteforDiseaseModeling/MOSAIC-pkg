---
name: suitability-panel-hidden-inputs
description: compile_suitability_data silently depends on pkg-side model/input/param_epidemic_peaks.csv (cases_binary) and on the panel's first row (poverty/spei/anomaly/heatwave fills); deaths_per_1000 is the WPP crude death rate, not cholera; explains non-obvious panel diffs
metadata:
  type: reference
---

Learned diffing the v0.100.0 panel rebuild (2026-09-30) against the 2026-09-21 panel:
- `cases_binary` comes from `get_cases_binary_from_peaks()`, which reads
  `PATHS$MODEL_INPUT/param_epidemic_peaks.csv` (MOSAIC-pkg/model/input, NOT MOSAIC-data). A panel
  built from another tree/branch's peaks file differs in ~650 cases_binary cells with identical
  cases. Re-running compile after est_epidemic_peaks is required for consistency.
- Dropping ONE edge week (the partial 1999-W52 row) perturbs whole columns: `spei_approx`
  (per-country scale() over the full series), every week-52 anomaly (climatology is grouped by
  iso x week: 27 yrs x 40 = 1080 rows), heatwave p95 threshold, and `poverty_ratio`.
- `poverty_ratio`: merged at panel years only, so a country whose first WB value is after 2000
  gets the GLOBAL MEAN (static-var mean fill), not NOCB from its pre-2000 value; the global mean
  itself moves with the row set (49.11 -> 48.96). ERI/SOM have no WB poverty at all.
- Panel key is (iso_code, date=Thursday); future horizon rows have NA date_start.
- `deaths_per_1000` is NOT cholera deaths: it is the UN WPP crude death rate (all-cause deaths per
  1,000 per year, interpolated daily; process_UN_demographics_data.R:78), a demographic covariate
  next to births_per_1000. A cholera-deaths change moves `deaths` (and `source_deaths` only if the source changes), never
  this column. The coordinator expected it to move with the ZAF 2023 deaths fix (2026-10-01).

Why: none of these show up as code changes; without this, a panel diff looks unexplained.
How to apply: when diffing panels, first check the peaks file provenance and the first/last row
set before attributing changes to fixes. Related: [[suitability-target-anchor-provenance]],
[[wb-processed-filename-drift]].
