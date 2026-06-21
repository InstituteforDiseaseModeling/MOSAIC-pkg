---
name: ai-source-integration-provenance
description: AI-mined surveillance source — schema, trust-tier discriminator, where it does/doesn't reach the fit target, and pre-clean status
metadata:
  type: reference
---

AI cholera source ingestion facts (verified 2026-06-19, local laptop).

**Pipeline already exists:** `process_AI_cholera_data()` reads AI repo integrated `cholera_weekly_{ISO}.csv` -> writes `MOSAIC-data/processed/cholera/ai/weekly/cholera_country_weekly_processed.csv` (cols: iso_code,country,year,week,cases,deaths,date_start,date_stop,month,confidence_weight,disaggregation_method). `process_cholera_surveillance_data(include_ai=TRUE)` merges it WHO>JHU>AI>SUPP and propagates both metadata cols into the combined WEEKLY file. Combined weekly already spans 2010-03-08 -> 2026-05-11.

**Trust-tier discriminator is `disaggregation_method`, NOT `confidence_weight`.** Verified: fourier_* rows carry confidence_weight up to 0.855-0.900, overlapping observed (0.63-1.0). The weight CANNOT separate synthetic from observed; the method tag (observed / documented_zero / assumed_zero / fourier_*) can. The row-level evidence tag (Documented_Absence vs Inferred_Absence) lives ONLY in row-level `cholera_data_ai.csv`, NOT the integrated weekly — needed only to split confirmed-vs-inferred zeros (all integrated documented_zero collapse to cw=0.80).

**Two-path gap (Lesson-#12 class):** config_default fit target (reported_cases/deaths) is built from WHO-ONLY daily (`make_config_default.R:402-427`, DATA_WHO_DAILY) with date_start=2023-02-01. include_ai=TRUE + confidence_weight + fit_date_start=2010 are ALREADY used as of config v4.0 — but ONLY in the psi/suitability/LSTM path, NOT the fit target. `process_AI_cholera_data()` is NOT called in LAUNCH.R; the existing DATA_AI_WEEKLY file is stale/manual.

**Daily downscale drops AI + metadata:** `process_cholera_surveillance_data()` daily block (~L248-250) NA-blanks ALL source=="AI" before downscale, and the daily output schema omits confidence_weight/disaggregation_method. To let curated AI reach the fit target: gate on disaggregation_method (keep observed, NA-blank fourier/assumed_zero), and carry both metadata cols into daily. confidence_weight is per-week constant -> REPLICATES over 7 days, never /7 (do not route through downscale_weekly_values).

**Pre-clean status:** KEN dup is already resolved in current files (raw 3131 distinct year-week 1971-2026; processed KEN=2870, 0 dup keys) — add a defensive duplicate-key assertion only. "Do-not-sum" cumulative rows + attribution-debt rows live only in row-level cholera_data_ai.csv, which MOSAIC never ingests; integrated weekly is already national + period-incremental.

See [[reference_who_surveillance_pipeline]] and [[who_field_semantics_gotchas]].
