---
name: who-surveillance-pipeline
description: End-to-end map of the WHO cholera surveillance pipeline (weekly + annual streams, sources, processors, outputs, model-input wiring)
metadata:
  type: reference
---

WHO cholera surveillance flows through TWO distinct streams; do not conflate them.

## Weekly stream (calibration time series)
- Upstream: WHO Global Cholera & AWD ArcGIS FeatureServer layer `cholera_adm0_week`
  (group-by epiwk,epiyr, sum of cases/deaths). Query built in
  `ees-cholera-mapping/who_awd.py` (READ-ONLY scraper repo). Python `process_all()`
  paginates per-country/per-field and writes `cholera_country_weekly.csv`.
- Lands at `ees-cholera-mapping/data/cholera/who/awd/cholera_country_weekly.csv`
  (= `PATHS$DATA_SCRAPE_WHO_WEEKLY`, get_paths.R:43). MOSAIC R reads it in place;
  NOT copied into MOSAIC-data/raw for the live path (a mirror exists at raw/WHO/weekly/).
- Raw schema: country (UPPERCASE adm0_name), year, week, cases_by_week, deaths_by_week (floats).
- `process_WHO_weekly_data()` (R/process_WHO_weekly_data.R): ISO-canonicalize, AFRO filter,
  rename cases_by_week->cases / deaths_by_week->deaths, MERGE spurious week-53 into week-52
  (calendar fix, not dedup), derive date_start/date_stop via ISOweek, drop ISOs with <=10 obs.
  Output: processed/WHO/weekly/cholera_country_weekly_processed.csv.

## Annual stream (CFR + priors, NOT the time-series likelihood)
- `process_WHO_annual_data()` (R/process_WHO_annual_data.R) assembles 3 tiers:
  1949-2021 = Our World in Data CSVs (raw/WHO/annual/who_global_1949_2021/); 2022 = hard-coded
  17-country df transcribed from WHO PDF; 2023+ = WHO ArcGIS dashboard item 3aa7bfec...
  (download.file with snapshot archiving + fallback to prior snapshots).
- Partial-year handling: `.who_annual_year_coverage()` derives calendar year from
  first_epiwk/last_epiwk, assigns cross-year snapshots to majority-day year, emits
  coverage_days + year_fraction. Dedup by (iso,year) keeping max coverage_days. CFR + binomial CI;
  rows with deaths>cases NA-ed. Output: processed/WHO/annual/who_afro_annual.csv.

## Multi-source merge
- `process_cholera_surveillance_data()` merges WHO/JHU/SUPP(/AI). Deterministic precedence
  WHO > JHU > AI > SUPP, with COMPLETENESS tie-break FIRST (both cases+deaths non-NA win before
  priority). Union-safe column harmonization. Square countries x all-Mondays panel; missing = NA
  never 0. Daily disaggregation via downscale_weekly_values (integer, remainder centered).
  AI rows NA-blanked before daily downscale (synthetic cases never reach the likelihood).
  include_ai defaults FALSE; AI confidence_weight NOT yet consumed downstream (loud warnings).
  Outputs: processed/cholera/weekly/cholera_surveillance_weekly_combined.csv,
  processed/cholera/daily/cholera_surveillance_daily_combined.csv.

## Model-input wiring (IMPORTANT split)
- config_default reported_cases/reported_deaths matrices are built in
  data-raw/make_config_default.R (L402-427) from processed/WHO/daily/cholera_country_daily_processed.csv
  -- the WHO-ONLY daily file produced by `downscale_weekly_cholera_data()`, NOT the multi-source
  combined daily file.
- The multi-source combined daily file feeds est_epidemic_peaks.R (peaks) and est_initial_E_I.R (IC).
- So: LASER observed-fit matrices = WHO-only daily; peaks + IC = WHO+JHU+SUPP combined daily. Real
  semantic seam worth flagging if a change intends the merged series to drive fit matrices.

## Orchestration
- model/LAUNCH.R section 2A (L127-133): process_WHO_annual_data -> process_CFR_data ->
  process_WHO_weekly_data -> process_JHU_weekly_data -> process_SUPP_weekly_data ->
  downscale_weekly_cholera_data -> process_cholera_surveillance_data. AI processor NOT in default seq.

See [[who-field-semantics-gotchas]] for unit/NA/source-precedence gotchas.
