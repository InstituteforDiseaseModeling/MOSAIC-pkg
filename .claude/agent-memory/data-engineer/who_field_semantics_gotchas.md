---
name: who-field-semantics-gotchas
description: Unit/NA/precedence gotchas across WHO/JHU/SUPP/AI cholera surveillance fields (Lesson #12 territory)
metadata:
  type: reference
---

Field-semantics gotchas in the cholera surveillance processors. Lesson #12 territory
(a field's unit/meaning being misread).

- Raw weekly fields are `cases_by_week`/`deaths_by_week` (UPPERCASE country, FLOAT weekly sums);
  renamed to `cases`/`deaths` in process_WHO_weekly_data. They are REPORTED counts.
- Observed `reported_cases`/`reported_deaths` are SURVEILLANCE counts; the engine (laser-cholera
  v0.13+) emits same-named `reported_*` outputs. Compare like-with-like; never observed reported_*
  vs simulated raw disease_*. (Cross-ref CLAUDE.md Lesson #12.)
- Missing-data convention DIFFERS by source: WHO and SUPP keep missing as NA; JHU
  (process_JHU_data.R:82-84) defaults missing cases to 0 (suspected sCh else confirmed cCh else 0)
  and missing deaths to 0. So JHU rows look "complete" even when truly unobserved -- relevant to the
  completeness tie-break in the merge.
- JHU `cases` = suspected (sCh) preferred over confirmed (cCh); not always confirmed.
- Dedup precedence in process_cholera_surveillance_data: WHO > JHU > AI > SUPP, but a COMPLETENESS
  tie-break (both cases AND deaths non-NA) is applied BEFORE source priority.
- Annual rows carry coverage_days + year_fraction; partial rolling current-year snapshots are KEPT
  (not mislabeled as full-year). Downstream CFR weighting relies on cases_total, not on dropping
  partials. Rows with deaths>cases are NA-ed.
- AI rows carry confidence_weight + disaggregation_method but downstream LSTM/likelihood do NOT yet
  consume confidence_weight; AI is NA-blanked out of the daily/likelihood path entirely.
- get_suspected_cases.R is NOT surveillance acquisition -- it builds chi (PPV) Beta priors from
  Weins et al 2023 -> param_chi_suspected_cases.csv (prior derivation, disease-modeler territory).

See [[who-surveillance-pipeline]] for the full pipeline map.
