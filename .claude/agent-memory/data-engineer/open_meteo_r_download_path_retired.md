---
name: open-meteo-r-download-path-retired
description: R-side Open-Meteo downloaders (download_climate_data, get_climate_future, get_climate_historical, set_openmeteo_api_key) were DELETED in v0.23.0; open-meteo-pipeline (Python) owns download. PR #51 edits the deleted files
metadata:
  type: project
---

`download_climate_data()`/`get_climate_future()` (plus get_climate_historical, set_openmeteo_api_key,
process_climate_data...) were deleted from MOSAIC-pkg in e5e93a100 (v0.23.0, 2026-04-14, issue #71).
Raw download now lives in the sibling Python repo `~/MOSAIC/open-meteo-pipeline` (same contributor,
DLukacevic-IDM), which already does batching, async concurrency, 429/5xx retry that honours Retry-After, and
free/paid (customer-*) endpoints. `process_open_meteo_data()` reads that repo's WIDE parquets from
`data/historical/{ISO}` (ERA5) + `data/climate/{model}/{ISO}` (CMIP6).

External PR #51 (open-meteo-perf, opened 2026-03/04, based on v0.17.33) modifies the deleted files and
writes LONG per-variable parquets to `DATA_RAW/climate/` (CMIP6 only, no ERA5), so its outputs are
unreadable by the live processor. A git merge gives modify/delete conflicts on 4 files.

**Why:** 2026-09-29 integration attempt stopped here; merging would resurrect 5 exported functions as a
parallel, disconnected download system (CLAUDE.md "don't leave two parallel systems").
**How to apply:** if asked to revive R-side Open-Meteo download, first get the user's decision on
supersede-vs-resurrect; point the contributor at open-meteo-pipeline instead. Related:
[[open-meteo-climate-horizon-provenance]], [[update_pipeline_raw_writer_compliance]].
