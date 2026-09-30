---
name: auto-cutoff-fabricated-zeros
description: lstm_v2 auto fit_date_stop = last ENSO-complete week (not last surveillance week); with response_var="transmission_intensity" NA cases -> 0 so ~36 post-surveillance weeks x 40 countries train as zeros
metadata:
  type: project
---

Found in the v0.99.9 deep review (2026-09-29, suitability component).

- `.est_suitability_lstm_v2` auto-detects `fit_date_stop` as `max(date[ENSO complete])`. The roxygen says "last date with both cholera cases and complete ENSO data", but the code never checks cases. On the canonical panel (checked 2026-09-29) that gives 2027-04-29, while the last surveillance week is 2026-08-13.
- `.psi_build_data` sets `cases[is.na] <- 0` BEFORE the intensity recipe runs, so under `response_var = "transmission_intensity"` the 1,440 post-surveillance rows (5.6% of the 25,640 train rows) enter training as observed zeros. They also enter the train-only `cases_99th`. The default target_D is safe because it propagates NA.
- In every mode, the manifest `fit_date_stop` and `data_psi_suitability.csv` `data_type == "training"` then label roughly 8 months with no targets as training.

**Why:** the auto cutoff looks harmless under the default target, and that hides it. The intensity branch is the only one documented as leak-free (train-only anchor), yet it is the branch this corrupts.
**How to apply:** when running the intensity target or reading a psi manifest, pass `fit_date_stop` explicitly. The default path also still carries the target-anchor leak ([[default-path-target-anchor-leak]]).
