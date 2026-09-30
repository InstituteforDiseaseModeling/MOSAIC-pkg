---
name: combiner-completeness-tiebreak-jhu-ai
description: process_cholera_surveillance_data completeness tie-break (complete cases+deaths beats source priority) + JHU missing-deaths=NA fix => 2235 observed JHU weeks lose to AI (mostly fourier) rows, cases 54k->265k; combined series HELD at v0.100.0 rebuild
metadata:
  type: project
---

Found 2026-09-30 in the v0.100.0 rebuild stage 1. `process_cholera_surveillance_data()` dedups per
(iso_code, date_start) ordering by `-.complete` (cases AND deaths non-NA) BEFORE source priority
WHO>JHU>AI>SUPP (R/process_cholera_surveillance_data.R ~176-184, pre-existing since v0.45.3).
Harmless while `process_JHU_weekly_data()` defaulted missing deaths to 0; once JHU deaths became NA
(8749 of 11740 JHU rows), every JHU week with an AI row of both fields loses to AI:
2235 source JHU->AI (2037 fourier_*, 100 documented_zero, 98 observed), 2011-2023, 34 countries,
those weeks' cases 54,248 -> 264,759 (4.9x). Deaths NA-valued->NA 6457 rows (expected part).

**Why:** the tie-break was designed when every source reported both fields; it now inverts the
documented precedence and replaces observed JHU cases with synthetic AI interpolation.
**How to apply:** do not commit a combined weekly/daily built by this combiner until it is changed
(options: tie-break on cases-completeness only, exclude fourier/imputed AI from winning over an
observed source, or field-wise coalesce deaths). Diagnose with source transition tables, per
[[surveillance-revision-is-source-precedence]]. Also noted same day: ai-cholera-data-mining sibling
repo has drifted — process_AI_cholera_data would write 72190 rows (was 64783; adds 1970-era
documented_zero weeks); AI processed file NOT refreshed in that stage.
