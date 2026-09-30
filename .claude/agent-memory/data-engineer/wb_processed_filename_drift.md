---
name: wb-processed-filename-drift
description: compile_suitability_data reads GDP_data_world_bank.csv / population_density_data_world_bank.csv but the processors write world_bank_GDP_data.csv / world_bank_population_density_data.csv — two WB covariates silently frozen at May-2025
metadata:
  type: project
---

Reader/writer filename mismatch in `MOSAIC-data/processed/world_bank/`
(observed 2026-09-17, pre-existing but newly consequential):

| covariate | writer emits | `compile_suitability_data.R` reads | match |
|---|---|---|---|
| GDP | `world_bank_GDP_data.csv` | `GDP_data_world_bank.csv` (L504) | NO |
| pop density | `world_bank_population_density_data.csv` | `population_density_data_world_bank.csv` (L519) | NO |
| urban pop | `world_bank_urban_population_data.csv` | same (L535) | yes |
| poverty | `world_bank_poverty_ratio_data.csv` | same (L551) | yes |

Both mismatched names still exist on disk as **May-2025 leftovers**, so
`if (file.exists(...))` is TRUE and the panel silently consumes 16-month-old GDP and
population density while urban/poverty refresh normally.

**Why it matters more now:** `update_mosaic_data()` runs all four `process_WB_*` steps
and reports `ok`, and `check_mosaic_data_freshness()` ages the *directory*, whose
newest file is today — so both tools report a clean refresh that two of four
covariates never received. On a clean checkout the stale files are absent, the
`file.exists()` guard fails silently, and `GDP`/`population_density` vanish from the
suitability panel entirely even though `est_suitability.R` lists both as covariates.

**How to apply:** when auditing WB/demographic refresh, verify the *consumer's* path
string, not just that the processor ran. Same failure shape as the orphaned
`processed/demographics/demographics_africa_2000_2023.csv` (dated 2024-09-20, no
writer anywhere in the package, read by `process_GTFCC_vaccination_data`,
`process_WHO_vaccination_data`, `est_vaccination_rate`, `est_mobility`) — refreshing
WPP does not reach any of those four.

**Still present at v0.93.0 (2026-09-29 review):** reads now at compile L569/L584; 15.0% of
shared GDP country-years differ >1% from the fresh file, 2025 missing. Default v7.3 feature set
has no WB covariates, so production psi impact is nil; legacy/"all" covariate path affected.

See [[newest-raw-mtime-resolver-hazard]] and [[wb-poverty-line-redefinition]].
