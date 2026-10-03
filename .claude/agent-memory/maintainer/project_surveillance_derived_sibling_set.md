---
name: surveillance-derived-sibling-set
description: When MOSAIC-data combined surveillance changes, peaks AND seasonal fits AND the psi panel must be regenerated together; how to scope psi-panel findings (2015+ fit window, NA-masked targets, account-agnostic backfill)
metadata:
  type: project
---

Learned verifying the v0.101 fix/v0101-trust red-team (2026-10-01).

**Sibling set for any change to `cholera_surveillance_weekly_combined.csv`:**
- `est_epidemic_peaks()` -> `model/input/param_epidemic_peaks.csv` + `data/epidemic_peaks.rda`
- `est_seasonal_dynamics()` -> `model/input/{param_seasonal_dynamics,pred_seasonal_dynamics_day,data_seasonal_precipitation}.csv`
  + DOCS_TABLES copies (precip CSV carries `cases`/`cases_scaled` too). Recorded args live in the
  `update_mosaic_data()` registry step 3A (2010-09-01..2025-09-01, min_obs 10, ward.D2, k 4, WHO/JHU/SUPP).
- the gitignored suitability panel (`compile_suitability_data()`).
The v0101-trust branch rebuilt peaks only (131b4359a); seasonal stayed on MOSAIC-data 93596a1, so
ZAF's envelope sat at 3 May/1.73 instead of 27 May/2.51 (ZAF cases seasonality is fit almost
entirely on 2023). Seasonal CSVs reach config/priors only at a defaults rebuild
(`make_config_default.R:302`, `make_priors_default.R:1073`), so staleness is silent until then.
**Outcome (v0.101.0):** seasonal dynamics were re-estimated on the shaped, report-dated surveillance
(16d6fd540) before config v6.1, and the panel backfill is off by default since 802fcb062, so the backfill
bullets below describe the opt-in path.
**Harness:** re-run `est_seasonal_dynamics()` with `PATHS$DATA_CHOLERA_WEEKLY` pointing at a scratch dir holding
`git show <sha>:processed/cholera/weekly/cholera_surveillance_weekly_combined.csv`, and
MODEL_INPUT/DOCS_TABLES/DOCS_FIGURES pointing at scratch. It takes about 15 s and reproduced the committed CSV bit-exact.

**Scoping a psi-panel finding before you rate its severity:**
- The production psi (v0.101 refit) fits from 2015-01-01 (`fit_date_start` NULL -> 2015). Pre-2015 panel rows reach
  psi only through the full-window anchors.
- lstm_v2 masks NA-target rows (`build_suitability_sequences.R` `unobserved`), so leaving a week NA drops it from
  training. The backfill's "false zero" rationale applies only to the legacy path.
- The combiner's emptying rules (annual-account cap, residue, curated drop) blank all 7 provenance columns. Only
  `cholera_surveillance_weekly_adjustments.csv` can tell "emptied" from "never reported".
- The panel backfill ignores the WHO annual account for every week, not only emptied ones: TCD 2019 overshoots a met
  account with no emptying involved.
- The floor population (`pop_rows` = non-AI rows) depends on which weeks carry AI rows. 11 countries are floor-bound,
  so include_ai-invariance claims fail for target_D in BFA and SWZ.
Related: [[reviewer-checklist]], [[project-integration-fixer-v0100]].
