---
name: ai-source-integration-provenance
description: AI-mined surveillance source — schema, trust-tier discriminator, how it reaches the fit target (combined daily since config v4.1, trust-tier gated; the 2026-06 psi-path-only gap is superseded), and pre-clean status
metadata:
  type: reference
---

AI cholera source ingestion facts (verified 2026-06-19, local laptop).

**Pipeline already exists:** `process_AI_cholera_data()` reads AI repo integrated `cholera_weekly_{ISO}.csv` -> writes `MOSAIC-data/processed/cholera/ai/weekly/cholera_country_weekly_processed.csv` (cols: iso_code,country,year,week,cases,deaths,date_start,date_stop,month,confidence_weight,disaggregation_method). `process_cholera_surveillance_data(include_ai=TRUE)` merges it WHO>JHU>AI>SUPP and propagates both metadata cols into the combined WEEKLY file. Combined weekly already spans 2010-03-08 -> 2026-05-11.

**Trust-tier discriminator is `disaggregation_method`, NOT `confidence_weight`.** Verified: fourier_* rows carry confidence_weight up to 0.855-0.900, overlapping observed (0.63-1.0). The weight CANNOT separate synthetic from observed; the method tag (observed / documented_zero / assumed_zero / fourier_*) can. The row-level evidence tag (Documented_Absence vs Inferred_Absence) lives ONLY in row-level `cholera_data_ai.csv`, NOT the integrated weekly — needed only to split confirmed-vs-inferred zeros (all integrated documented_zero collapse to cw=0.80).

**Two-path gap (Lesson-#12 class) — SUPERSEDED:** since config v4.1 the fit target reads the combined daily file (`cholera_surveillance_daily_combined.csv`, include_ai = TRUE; make_config_default.R), and the blanket source == "AI" daily blank was replaced by a trust-tier gate (process_cholera_surveillance_data); since v0.101.0 imputed rows only fill the gap to the WHO account of the year and carry reported_tier 3. The 2026-06-19 record follows: config_default fit target (reported_cases/deaths) is built from WHO-ONLY daily (`make_config_default.R:402-427`, DATA_WHO_DAILY) with date_start=2023-02-01. include_ai=TRUE + confidence_weight + fit_date_start=2010 are ALREADY used as of config v4.0 — but ONLY in the psi/suitability/LSTM path, NOT the fit target. `process_AI_cholera_data()` is NOT called in LAUNCH.R; the existing DATA_AI_WEEKLY file is stale/manual.

**Daily downscale drops AI + metadata (2026-06-19; superseded by the trust-tier gate):** `process_cholera_surveillance_data()` daily block (~L248-250) NA-blanks ALL source=="AI" before downscale, and the daily output schema omits confidence_weight/disaggregation_method. To let curated AI reach the fit target: gate on disaggregation_method (keep observed, NA-blank fourier/assumed_zero), and carry both metadata cols into daily. confidence_weight is per-week constant -> REPLICATES over 7 days, never /7 (do not route through downscale_weekly_values).

**Pre-clean status:** KEN dup is already resolved in current files (raw 3131 distinct year-week 1971-2026; processed KEN=2870, 0 dup keys) — add a defensive duplicate-key assertion only. "Do-not-sum" cumulative rows live in row-level cholera_data_ai.csv, but the integrated weekly is NOT always period-incremental (CORRECTED 2026-10-01): NGA 2023-W21 `observed` 1,851/52 is a year-to-date total, and fourier rows double count WHO-reported outbreaks; see [[ai-fourier-full-total-double-count]].

See [[reference_who_surveillance_pipeline]] and [[who_field_semantics_gotchas]].
