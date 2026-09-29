---
name: wb-poverty-line-redefinition
description: World Bank SI.POV.DDAY silently changed meaning ($2.15/2017-PPP -> $3.00/2021-PPP) between the 2025-04 portal export on disk and the live API; same code, same field name, +8.75pp level shift for MOSAIC-40
metadata:
  type: project
---

`SI.POV.DDAY` is **not a stable definition**. The World Bank rebased the international
poverty line in its 2025 revision and kept the indicator code unchanged:

- `MOSAIC-data/raw/world_bank/poverty_ratio/API_SI.POV.DDAY_DS2_en_csv_v2_86770.csv`
  (portal export, `Last Updated Date` 2025-04-15) — Indicator Name =
  **"Poverty headcount ratio at $2.15 a day (2017 PPP) (% of population)"**
- Live Indicators API (`lastupdated` 2026-07-13) — Indicator Name =
  **"Poverty headcount ratio at $3.00 a day (2021 PPP) (% of population)"**

Measured 2026-09-17 over 198 shared MOSAIC-40 country-year observations:
**0 unchanged**, mean level 44.7% -> 53.4% (**+8.75 pp**), median ratio 1.213,
157/198 shift by >5 pp. Examples: KEN 2021 36.1 -> 46.4; COD 2020 78.9 -> 85.3.

**Why:** this is the canonical Lesson-#12 shape — the field name (`poverty_ratio`),
the indicator code, and the column dtype are all identical; only the *meaning* moved.
`process_WB_poverty_ratio_data()` discards the `Indicator Name` column, so the
processed artifact `world_bank_poverty_ratio_data.csv` carries **no trace** of which
poverty line it encodes. `poverty_ratio` is a live LSTM covariate
(`est_suitability.R`, `run_rolling_cv_suitability.R`), so the switch propagates
straight into psi.

**How to apply:** any refresh of WB poverty data is a *covariate redefinition*, not a
vintage bump — it needs a psi re-fit and a note in `MOSAIC-docs/03-data.Rmd`, not a
silent overwrite. Persist `Indicator Name` (or a `poverty_line` / `ppp_year` column)
into the processed output so the definition is auditable. Same risk class applies to
any WB indicator whose title encodes a threshold or base year.

Also note the units misnomer: `poverty_ratio` and `urban_pop_prop` both hold
**percent (0-100)**, not a ratio/proportion. Range on disk: poverty 0-97,
urban 1.84-100. `compile_suitability_data()` renames urban to
`urban_population_pct` (correct); `poverty_ratio` keeps the misleading name
all the way into the covariate list.

See [[newest-raw-mtime-resolver-hazard]] for how a stale vintage can silently win
the file-selection race and mask (or unmask) this change.
