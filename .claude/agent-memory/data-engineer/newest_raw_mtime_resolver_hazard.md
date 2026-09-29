---
name: newest-raw-mtime-resolver-hazard
description: .wb_newest_raw()/.wpp_newest_raw() pick the raw input by file MTIME; a plain cp or an mtime tie deterministically reverts the pipeline to the OLD vintage, silently and with no provenance record
metadata:
  type: project
---

`R/download_WB_data.R::.wb_newest_raw()` and `R/download_UN_WPP_data.R::.wpp_newest_raw()`
resolve which raw file the `process_WB_*` / `process_UN_demographics_data` processors
read by `order(file.info(hits)$mtime, decreasing = TRUE)[1]`. Two measured failure modes
(verified 2026-09-17):

1. **`file.copy()` / `cp` without `-p` sets destination mtime to now.** Copying a
   2025-vintage portal export into `raw/world_bank/GDP/` immediately outranked an API
   pull written seconds earlier. Processed GDP silently reverted from 1960-2025 /
   17,160 rows to 1960-2024 / 17,290 rows. No message, no error.
2. **On an exact mtime tie the OLD file wins.** `order()` is stable and `list.files()`
   returns alphabetical order, so `API_..._v2_132025.csv` beats `API_..._v2_api_<date>.csv`
   ("1" < "a") and `UN_..._1967_2100_birth_rate.csv` beats
   `UN_..._birth_rate_wpp2024_<date>.csv` ("1" < "b"). Any `rsync -a`, `tar -x`,
   `unzip`, cloud-sync rehydrate, or archive restore that equalises mtimes therefore
   reverts *deterministically* to the pre-API vintage.

**Why it is invisible:** neither processor logs the resolved input path, and nothing is
written into the processed artifact recording which raw vintage produced it.
`check_mosaic_data_freshness()` cannot catch it either — it ages the *output* mtime,
which is fresh because the processor just ran.

**How to apply:** treat mtime as a tiebreaker of last resort, never the primary key.
Prefer ranking on the date embedded in the filename (API pulls carry `_api_<ISO date>`;
WPP pulls carry `_wpp<rev>_<ISO date>`) and fall back to mtime only for the opaque
portal vintage ids. At minimum, `message()` the selected path and stamp the source
filename + retrieval date into the processed output. Relevant to
[[wb-poverty-line-redefinition]]: with the resolver in this state you cannot tell from
the artifact whether `poverty_ratio` is the $2.15 or the $3.00 series.
