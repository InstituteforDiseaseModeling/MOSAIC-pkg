---
name: covid-surveillance-zerowalls
description: config_default (2018 build) carries MOZ 2020-21 + NGA 2020 as FULL-WEIGHT hard-zero surveillance-gap walls; biases beta/psi down in expanding-window fits; likelihood now HAS per-cell weight slot
metadata:
  type: reference
---

In the shipped 2018-01-01 `config_default` daily grid (verified live 2026-07-06), COVID-era
WHO AFRO reporting gaps are encoded as HARD ZEROS at weight = 1.0, not NA/down-weighted:
- MOZ: 2020 AND 2021 = 0 cases, weight 1.0 (731 days). 2018-19 also sparse.
- NGA: 2020 = 0 cases, weight 1.0 (366 days).
- COD: continuous (endemic) — fine. ETH: continuous from 2019; 2020-21 already carry weight 0.5.

These are almost certainly surveillance gaps (MOZ had ongoing post-Cyclone-Idai transmission
through 2020), NOT true epidemiological absence. In an EXPANDING-window fit (e.g. forecast-CV
from 2018-01) they force calibration to suppress beta_j0/psi to explain 1-2 yr of "zero cholera",
biasing baseline transmission + reporting DOWNWARD and UNEQUALLY across countries (MOZ worst,
COD immune) — contaminates cross-country skill comparisons.

FIX is cheap: calc_model_likelihood NOW supports a per-cell weight matrix `weights_obs_cases`
(the gap flagged in [[project_multisource_2023_integration_epi_review]] has since CLOSED). Options:
(1) down-weight/NA-mask MOZ 2020-01..2021-12 + NGA 2020 cells in reported_cases_weight /
    reported_deaths_weight of the run's base_config (treat known gaps as missing, cite COVID AFRO
    disruption); or (2) window the training start to 2021-07-01 via base_config_rds (do NOT mutate
    date_start — misaligns date-indexed arrays). Raised in the OCV-4 forecast-CV red-team.
