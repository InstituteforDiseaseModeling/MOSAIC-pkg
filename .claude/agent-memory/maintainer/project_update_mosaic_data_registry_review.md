---
name: update-mosaic-data-registry-review
description: Review of the 51-step update_mosaic_data() registry + check_mosaic_data_freshness (v0.90.6) — dead include_suitability guard, phantom/missing dep edges, files with no producer
metadata:
  type: project
---

Adversarial review of `R/update_mosaic_data.R` + `R/check_mosaic_data_freshness.R`
(uncommitted, v0.90.5 -> 0.90.6, 2026-09-17). Verdict was REQUEST-CHANGES.

**Why:** these supersede the gitignored `model/LAUNCH.R` as the canonical data pipeline, so
the registry becomes the authoritative dependency graph. Findings below are durable facts
about the pipeline, not about that one diff.

**How to apply:** re-check these on any change to the step registry, the `deps` vectors, or
the newest-wins resolvers.

## The registry's group ids are "1A".."4B", never "4"
Both `.mosaic_select_steps()` and `list_mosaic_data_steps()` filtered with
`s$group != "4"`, which matches nothing — `include_suitability = FALSE` was a **no-op** and
the default call silently included the hours-long, 10-seed TensorFlow `est_suitability()`
while printing "group 4 excluded". Same shape as Lesson #13 (a guard that can never fire).
`skip = "4"` DOES work because `match_any()` also tests `substr(group, 1, 1)`.
Any future group filter must compare the prefix, not the whole id.

## Processed files with NO producer in R/ (four+ consumers each)
- `processed/demographics/demographics_africa_2000_2023.csv` (frozen 2024-09-20). Read by
  `est_vaccination_rate`, `process_GTFCC_vaccination_data`, `est_mobility`,
  `process_WHO_vaccination_data`. `process_UN_demographics_data()` writes
  `UN_world_population_prospects_{annual,daily,1967_2100}.csv` — **not** this file. So any
  registry edge `X <- process_UN_demographics_data` for those four consumers is FALSE:
  re-running the producer does not refresh what they read.
- `processed/geography/country_centroids.csv` + `country_regions.csv` — directory does not
  exist; `compile_suitability_data()` reads both behind `if (file.exists())`, so lat/lon is
  permanently absent and region silently falls back to `get_who_region()`.

## Dependency edges that were wrong (verify these stay fixed)
- `compile_suitability_data` declared `est_vaccination_rate` — but the vaccination block was
  **removed in v0.30.26**; compile reads no vaccination file. Phantom edge; a failing
  `est_vaccination_rate` needlessly blocks all of group 4.
- Missing: the 4 `process_WB_*` outputs, `WASH_data_Sikder_2023.csv`, and
  `country_elevation_mean.csv` — all read by `compile_suitability_data` behind
  `if (file.exists())`, i.e. silently skipped. (Mitigating: none are in the production
  `feature_set = "v7.3"` 38-feature set, so the shipped psi path is unaffected; they matter
  for `feature_set = "default"` and ablations.)
- Missing shapefile edges: `AFRICA_ADM0.shp` is read **unguarded** (`sf::st_read`) by
  `est_seasonal_dynamics` (~line 250) and `process_OAG_data`; `<iso>_ADM0.shp` by
  `download_country_DEM` and `get_elevation`. `download_africa_shapefile` had **zero**
  declared dependents.

## Download steps are hard gates in front of resolver-backed processors
`download_UN_WPP_data` failing blocks **8** steps (demographics -> GTFCC -> combine ->
vaccination rate -> compile -> est_suitability, plus est_mobility, est_demographic_rates) —
and it is the one downloader with a bare `utils::download.file()` (no tryCatch), so a network
blip cascades. Yet `.wpp_newest_raw()` would have happily resolved the existing raw file.
`download_WB_data` / `download_IDMC_data` swallow per-item errors and return `ok = FALSE`
frames, so they report **"ok"** to the driver even on total outage (a green that means
"downloaded nothing").

## Newest-wins resolver traps
- `.wpp_newest_raw` / `.wb_newest_raw` rank by **mtime** only. `touch` promotes a stale file;
  on an exact mtime tie `order()` is stable so the alphabetically-first (= legacy) file wins.
  The legacy WPP portal export has 27 columns and 3 duplicate rows per (iso, year) with
  identical Value; the API pull has 5 columns and 1 row — schema-compatible for
  `process_UN_demographics_data` (it takes Iso3/Time/Value + `distinct()`), but coverage
  narrows 58 -> 54 ISOs (API defaults to `iso_codes_africa`; all 40 MOSAIC countries survive).
- `process_EMDAT_data`'s resolver uses `regmatches(candidates, regexpr(...))`, which **drops**
  non-matching elements, so `order_keys` can be shorter than `candidates` and the wrong file
  is selected with no warning. Demonstrated. Pre-existing, but widening the pattern to
  `.(xlsx|csv)` increased exposure. Also `regexpr` runs on the **full path**, so a date-like
  parent directory name would be picked up.
- `.idmc_latest_snapshot` ranks by directory NAME (ISO date) — correct — but
  `download_IDMC_data` writes a snapshot dir even when most of the 38 per-country fetches
  fail, so a partial snapshot silently supersedes yesterday's complete one.

## check_mosaic_data_freshness: inverted safety on non-git dirs
A directory that exists but is **not a git repo** yields `last_commit = NA` ->
`stale = !is.na(age) && ...` = FALSE -> printed `[ok     ]`. An *absent* repo is correctly
`stale = TRUE`. Verified empirically against a fake root. Also: `area` is
`basename(dirname(tgt))`, so `MOSAIC-pkg/model/input` labels as bare `model/` and the
`processed/` segment is dropped from every `MOSAIC-data` row.

## Related
[[reviewer-checklist]], [[rcmdcheck-baseline-v084]], [[config-default-psi-provenance]]
