---
name: surveillance-revision-is-source-precedence
description: When combined-surveillance cells change between refreshes it is USUALLY AI/Fourier gap-fill being superseded by arriving WHO weekly data (source precedence), not WHO revising its own counts — diagnose with source + disaggregation_method + confidence_weight, never with the case value alone
metadata:
  type: reference
---

Measured on the 2026-09-18 surveillance refresh (507 `cases` cells changed on
shared (iso_code, date) keys between the 2026-09-17 and 2026-09-21 suitability
panel builds):

| transition | cells |
|---|---|
| source AI -> WHO | 379 |
| source JHU -> WHO | 38 |
| source NA -> WHO | 9 |
| source WHO -> WHO (a genuine WHO restatement) | 81 |

363 of the 507 carried a `disaggregation_method` of `fourier_country_k1/k2/k3` or
`fourier_regional_*` before and `NA` after — i.e. a synthetic weekly
disaggregation of an AI-mined annual total was replaced by a real WHO weekly
report. `confidence_weight` went from 0.33-0.9 to 1.0 on 388 of them.

**So only ~16% of "revisions" are a source restating its own numbers.** The rest
are the documented precedence ladder firing as better data arrives.

The direction is often drastic, not incremental: UGA 343.8 -> 0 cases over 71
cells, TGO 209 -> 0 over 23, GHA 2533 -> 0 over 15, KEN 15592 -> 7651 over 86.
AI/Fourier gap-fill had invented case-weeks where WHO later reported zero.

**How to apply:** when a processed surveillance cell changes, join
`source` + `disaggregation_method` + `confidence_weight` old-vs-new before
concluding anything about data quality. A claim of the form "the source revised
history" needs the WHO->WHO subset to support it. Corollary: freezing an older
panel to preserve comparability freezes the *synthetic* values in preference to
the real ones that superseded them — state that cost explicitly.

Related: [[ai_source_integration_provenance]], [[who_field_semantics_gotchas]],
[[suitability_target_anchor_provenance]].
